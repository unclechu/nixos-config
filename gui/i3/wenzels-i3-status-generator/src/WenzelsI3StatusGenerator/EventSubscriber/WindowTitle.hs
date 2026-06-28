-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE UnicodeSyntax, GHC2024, QuasiQuotes, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot, OverloadedStrings #-}

module WenzelsI3StatusGenerator.EventSubscriber.WindowTitle
     ( WindowTitle (..)
     , subscribeToFocusedWindowTitleUpdates
     ) where

import Control.Applicative ((<|>))
import qualified Control.Concurrent.Async as Async
import Control.Monad (forever)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (typeMismatch)
import Data.ByteString.Char8 (hGetLine)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Int (Int64)
import Data.Maybe (listToMaybe, mapMaybe)
import GHC.Generics (Generic)
import qualified System.Process.Typed as Proc
import Text.InterpolatedString.QM (qn, qm)
import WenzelsI3StatusGenerator.Utils ((∘), (•), (⋄), (<&>), (&))
import WenzelsI3StatusGenerator.Utils.Aeson (withFieldNamer)


subscribeToFocusedWindowTitleUpdates
  ∷ (Maybe WindowTitle → IO ())
  -- ^ Focused winodw title update callback
  → IO (Maybe WindowTitle, Async.Async ())
  -- ^ Current/initial focused window title and thread handle
subscribeToFocusedWindowTitleUpdates updateCallback = do
  (initialTree ∷ Either String WindowTree) ←
    fmap (Aeson.eitherDecodeStrict' ∘ LBS.toStrict)
      ∘ Proc.readProcessStdout_
      $ Proc.proc "i3-msg" ["-t", "get_tree"]
      & Proc.setStdin Proc.closed
      & Proc.setStderr Proc.inherit

  (initTitle ∷ Maybe WindowTitle) ←
    case initialTree of
      Left msg →
        Nothing <$ fail [qm| Error while parsing i3 window tree: {msg} |]
      Right tree → pure $ let
        f x@WindowTree { focused = True } = Just x
        f WindowTree { nodes } = listToMaybe $ mapMaybe f nodes
        in f tree >>= mkWindowTitle

  threadHandle ← Async.async $ do
    let
      procSpec
        = Proc.proc "i3-msg" ["-t", "subscribe", "-m", [qn| ["window", "workspace"] |]]
        & Proc.setStdin Proc.closed
        & Proc.setStdout Proc.createPipe
        & Proc.setStderr Proc.inherit
    Proc.withProcessWait procSpec $ \p → do
      forever @IO @() @() $ hGetLine (Proc.getStdout p)
        <&> Aeson.eitherDecodeStrict'
        >>= either (fail ∘ ("Error while parsing window title event: " ⋄)) pure
        >>= getFocusedWindowTitle • \case
              Ignore → pure ()
              FocusedWindowNotFound → updateCallback Nothing
              FocusedWindowClosed → updateCallback Nothing
              FocusedWindowTitle title → updateCallback title

  pure (initTitle, threadHandle)


getFocusedWindowTitle ∷ ChangeEvent → EventResolve
getFocusedWindowTitle = \case
  WindowFocusEvent container →
    if not container.focused
    then Ignore
    else FocusedWindowTitle ∘ mkWindowTitle $ container
  WindowTitleEvent container →
    if not container.focused
    then Ignore
    else FocusedWindowTitle ∘ mkWindowTitle $ container
  WindowCloseEvent container →
    if container.focused
    then FocusedWindowClosed
    else Ignore

  WorkspaceFocusEvent current → let
    f x@WindowTree { focused = True } = Just x
    f WindowTree { nodes } = firstFocused nodes

    firstFocused = listToMaybe ∘ mapMaybe f
    top = firstFocused current.nodes

    in maybe FocusedWindowNotFound (FocusedWindowTitle ∘ mkWindowTitle) top

  OtherEvent _ → Ignore


-- * Helpers

-- | Make "WindowTitle" out of window properties
mkWindowTitle ∷ HasWindowProperties a ⇒ a → Maybe WindowTitle
mkWindowTitle = getWindowProperties • (>>= title) • fmap WindowTitle


-- * Types

newtype WindowTitle = WindowTitle { unWindowTitle ∷ String }
  deriving (Show, Eq)


data EventResolve
  = Ignore
  | FocusedWindowNotFound
  | FocusedWindowClosed
  | FocusedWindowTitle (Maybe WindowTitle)
  deriving (Show, Eq)


data ChangeEvent
  = WindowFocusEvent EventContainer
  | WindowTitleEvent EventContainer
  | WindowCloseEvent EventContainer
  | WorkspaceFocusEvent EventWorkspace
  | OtherEvent Aeson.Value
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON ChangeEvent where
  parseJSON json@(Aeson.Object obj) =
    case KM.lookup "change" obj of
      Nothing → mismatch
      Just "focus" →
        maybe mismatch (fmap WindowFocusEvent ∘ Aeson.parseJSON) (KM.lookup containerKey obj)
        <|> maybe mismatch (fmap WorkspaceFocusEvent ∘ Aeson.parseJSON) (KM.lookup "current" obj)
        -- ↑ When there’s “current” there can be also optional “old” value
      Just "title" →
        maybe mismatch (fmap WindowTitleEvent ∘ Aeson.parseJSON) (KM.lookup containerKey obj)
      Just "close" →
        maybe mismatch (fmap WindowCloseEvent ∘ Aeson.parseJSON) (KM.lookup containerKey obj)
      Just _ →
        pure $ OtherEvent json
    where
      containerKey = "container"
      mismatch = typeMismatch "ChangeEvent" json
  parseJSON json = typeMismatch "ChangeEvent" json


data EventContainer
  = EventContainer
  { id ∷ Int64
  , _type ∷ String
  , focused ∷ Bool
  , urgent ∷ Bool
  , output ∷ String
  , layout ∷ String
  , name ∷ Maybe String
  , window ∷ Int64
  , windowProperties ∷ Maybe EventContainerWindowProperties
  , sticky ∷ Bool
  }
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON EventContainer where
  parseJSON = Aeson.genericParseJSON $ withFieldNamer f where
    f ('_' : xs) = xs; f x = x

instance HasWindowProperties EventContainer where
  getWindowProperties = (.windowProperties)


data EventContainerWindowProperties
  = EventContainerWindowProperties
  { _class ∷ Maybe String
  , _instance ∷ Maybe String
  , title ∷ Maybe String
  , windowRole ∷ Maybe String
  }
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON EventContainerWindowProperties where
  parseJSON = Aeson.genericParseJSON $ withFieldNamer f where
    f ('_' : xs) = xs; f x = x


data EventWorkspace
  = EventWorkspace
  { id ∷ Int64
  , _type ∷ String
  , focused ∷ Bool
  , urgent ∷ Bool
  , output ∷ String
  , layout ∷ String
  , name ∷ String
  , nodes ∷ [WindowTree]
  , sticky ∷ Bool
  }
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON EventWorkspace where
  parseJSON = Aeson.genericParseJSON $ withFieldNamer f where
    f ('_' : xs) = xs; f x = x


-- | i3 windows tree
data WindowTree
  = WindowTree
  { id ∷ Int64
  , focused ∷ Bool
  , urgent ∷ Bool
  , layout ∷ String
  , output ∷ Maybe String
  , windowProperties ∷ Maybe EventContainerWindowProperties
  , nodes ∷ [WindowTree]
  }
  deriving (Show, Eq, Generic)

instance Aeson.FromJSON WindowTree where
  parseJSON = Aeson.genericParseJSON $ withFieldNamer Prelude.id

instance HasWindowProperties WindowTree where
  getWindowProperties = (.windowProperties)


class HasWindowProperties a where
  getWindowProperties ∷ a → Maybe EventContainerWindowProperties
