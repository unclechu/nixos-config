-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedRecordDot #-}

module WenzelsI3StatusGenerator.EventSubscriber.WindowTitle
     ( WindowTitle (..)
     , subscribeToFocusedWindowTitleUpdates
     ) where

import "base" GHC.Generics (Generic)

import "aeson" Data.Aeson.Types (typeMismatch)
import "base" Data.Int (Int64)
import "base" Data.Maybe (listToMaybe, catMaybes)
import "bytestring" Data.ByteString (hGetLine, hGetContents)
import "qm-interpolated-string" Text.InterpolatedString.QM (qn, qm)
import qualified "aeson" Data.Aeson.KeyMap as KM

import "aeson" Data.Aeson
  ( Value (Object)
  , FromJSON (..)
  , genericParseJSON
  , eitherDecodeStrict'
  )

import "base" Control.Applicative ((<|>))
import "base" Control.Monad (forever)
import qualified "async" Control.Concurrent.Async as Async

import "process" System.Process
  ( CreateProcess (std_in, std_out, std_err)
  , StdStream (NoStream, Inherit, CreatePipe)
  , proc
  , waitForProcess
  , withCreateProcess
  )

-- Local imports

import WenzelsI3StatusGenerator.Utils
import WenzelsI3StatusGenerator.Utils.Aeson (withFieldNamer)


subscribeToFocusedWindowTitleUpdates
  ∷ (Maybe WindowTitle → IO ())
  -- ^ Focused winodw title update callback
  → IO (Maybe WindowTitle, Async.Async ())
  -- ^ Current/initial focused window title and thread handle
subscribeToFocusedWindowTitleUpdates updateCallback = do
  (initialTree ∷ Either String WindowTree) ← do
    let
      procSpec =
        (proc "i3-msg" ["-t", "get_tree"])
          { std_in  = NoStream
          , std_out = CreatePipe
          , std_err = Inherit
          }
    withCreateProcess procSpec $ \Nothing (Just hOut) _ procHandle → do
      result ← eitherDecodeStrict' <$> hGetContents hOut
      result <$ waitForProcess procHandle

  (initTitle ∷ Maybe WindowTitle) ←
    case initialTree of
      Left msg →
        Nothing <$ fail [qm| Error while parsing i3 window tree: {msg} |]
      Right tree → pure $ let
        f x@WindowTree { focused = True } = Just x
        f WindowTree { nodes } = listToMaybe $ catMaybes $ f <$> nodes
        in f tree >>= mkWindowTitle

  threadHandle ← Async.async $ do
    let
      procSpec =
        (proc "i3-msg" ["-t", "subscribe", "-m", [qn| ["window", "workspace"] |]])
          { std_in  = NoStream
          , std_out = CreatePipe
          , std_err = Inherit
          }
    withCreateProcess procSpec $ \Nothing (Just hOut) _ _ →
      forever @IO @() @() $ hGetLine hOut
        <&> eitherDecodeStrict'
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

    firstFocused = listToMaybe ∘ catMaybes ∘ map f
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
  | OtherEvent Value
  deriving (Show, Eq, Generic)

instance FromJSON ChangeEvent where
  parseJSON json@(Object obj) =
    case KM.lookup "change" obj of
      Nothing → mismatch
      Just "focus" →
        maybe mismatch (fmap WindowFocusEvent ∘ parseJSON) (KM.lookup containerKey obj)
        <|> maybe mismatch (fmap WorkspaceFocusEvent ∘ parseJSON) (KM.lookup "current" obj)
        -- ↑ When there’s “current” there can be also optional “old” value
      Just "title" →
        maybe mismatch (fmap WindowTitleEvent ∘ parseJSON) (KM.lookup containerKey obj)
      Just "close" →
        maybe mismatch (fmap WindowCloseEvent ∘ parseJSON) (KM.lookup containerKey obj)
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

instance FromJSON EventContainer where
  parseJSON = genericParseJSON $ withFieldNamer f where
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

instance FromJSON EventContainerWindowProperties where
  parseJSON = genericParseJSON $ withFieldNamer f where
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

instance FromJSON EventWorkspace where
  parseJSON = genericParseJSON $ withFieldNamer f where
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

instance FromJSON WindowTree where
  parseJSON = genericParseJSON $ withFieldNamer Prelude.id

instance HasWindowProperties WindowTree where
  getWindowProperties = (.windowProperties)


class HasWindowProperties a where
  getWindowProperties ∷ a → Maybe EventContainerWindowProperties
