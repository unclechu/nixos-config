-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE UnicodeSyntax, GHC2024, QuasiQuotes, DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot, ViewPatterns, OverloadedStrings #-}

module WenzelsI3StatusGenerator.EventSubscriber.IPC
     ( IPCEvent (..)
     , XmonadrcIfaceParams (..)
     , XlibKeysHackIfaceParams (..)
     , subscribeToIPCEvents
     ) where

import Control.Arrow ((&&&))
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally)
import Control.Monad (when, void, forever)
import Data.Default (Default (def))
import Data.Word (Word8)
import qualified DBus
import qualified DBus.Client
import Text.InterpolatedString.QM (qm)
import WenzelsI3StatusGenerator.Layout (Layout (..), numToLayout)
import WenzelsI3StatusGenerator.Utils ((≢), (&), (•), (∘), (<&>), (⋄))


subscribeToIPCEvents
  ∷ (∀a. (String → a) → a)
  -- ^ Function that provides current display marker
  → (IPCEvent → IO ())
  -- ^ Value update callback
  → IO (DBus.Signal → IO (), Async.Async ())
  -- ^ Emit function and the thread handle
subscribeToIPCEvents withDisplayMarker eventCallback = do
  client ← DBus.Client.connectSession

  do -- Grab the bus name for the application
    let name = withDisplayMarker (def ∷ XmonadrcIfaceParams).busName
    reply ← DBus.Client.requestName client name []
    when (reply ≢ DBus.Client.NamePrimaryOwner) $
      fail [qm| Requesting name '{name}' error: {reply} |]

  -- Listen to the events
  signalHandlers ←
    let
      listen member handler
        = (DBus.signalBody • handler)
        & DBus.Client.addMatch
            client
            basicMatchRule { DBus.Client.matchMember = Just member }
    in
      sequence
        [ listen "numlock" $ \case
            [DBus.fromVariant → Just (x ∷ Bool)] → eventCallback ∘ NumLock $ x
            _ → pure () -- Ignore incorrect arguments

        , listen "capslock" $ \case
            [DBus.fromVariant → Just (x ∷ Bool)] → eventCallback ∘ CapsLock $ x
            _ → pure () -- Ignore incorrect arguments

        , listen "xkblayout" $ \case
            [DBus.fromVariant → Just (x ∷ Word8)] →
              eventCallback ∘ KbdLayout ∘ maybe (Left x) Right ∘ numToLayout $ x
            _ → pure () -- Ignore incorrect arguments

        , listen "alternative_level" $ \case
            [ DBus.fromVariant → Just (level ∷ Word8),
              DBus.fromVariant → Just (isPermanent ∷ Bool) ] →
              eventCallback ∘ Alternative $
                if level > minBound
                then Just (level, isPermanent)
                else Nothing
            _ → pure () -- Ignore incorrect arguments
        ]

  -- If “xlib-keys-hack” started before ask it to reflush indicators.
  -- It’s important to put if *after* the listeners attachment.
  DBus.Client.emit client
    ( DBus.signal
        (withDisplayMarker (def ∷ XmonadrcIfaceParams).flushObjPath)
        (def ∷ XmonadrcIfaceParams).interfaceName
        "request_flush_all"
    )
    { DBus.signalSender =
        Just ∘ withDisplayMarker $ (def ∷ XmonadrcIfaceParams).busName
    , DBus.signalDestination = Nothing
    , DBus.signalBody = []
    }

  (emitSignal, getNextEmitterTask) ← newEmptyMVar <&> putMVar &&& takeMVar

  threadHandle ← Async.async $
    let
      finalizer = do
        mapM_ (DBus.Client.removeMatch client) signalHandlers

        void -- Ignoring the reply, just trying to release the name
          ∘ DBus.Client.releaseName client
          ∘ withDisplayMarker
          $ (def ∷ XmonadrcIfaceParams).busName

        DBus.Client.disconnect client

      handleTask = getNextEmitterTask >>= DBus.Client.emit client
    in
      forever @IO @() @() handleTask `finally` finalizer

  pure (emitSignal, threadHandle)

  where
    basicMatchRule = DBus.Client.matchAny
      { DBus.Client.matchPath =
          Just (def ∷ XmonadrcIfaceParams).objPath
      , DBus.Client.matchInterface =
          Just (def ∷ XmonadrcIfaceParams).interfaceName
      , DBus.Client.matchDestination =
          Just ∘ withDisplayMarker $ (def ∷ XmonadrcIfaceParams).busName
      }


-- * Types

data IPCEvent
  = NumLock Bool
  | CapsLock Bool
  | KbdLayout (Either Word8 Layout)
  -- ^ @Left@ when failed to parse @Layout@, providing the raw value instead
  | Alternative (Maybe (Word8, Bool))
  -- ^ @Nothing@ means alternative mode is turned off
  deriving (Show, Eq)


data XmonadrcIfaceParams
  = XmonadrcIfaceParams
  { objPath ∷ DBus.ObjectPath
  , flushObjPath ∷ String → DBus.ObjectPath
  , busName ∷ String → DBus.BusName
  , interfaceName ∷ DBus.InterfaceName
  }

instance Default XmonadrcIfaceParams where
  def
    = XmonadrcIfaceParams
    { objPath = "/"
    , flushObjPath  = DBus.objectPath_ ∘ ("/com/github/unclechu/xmonadrc/" ⋄)
    , busName = DBus.busName_ ∘ ("com.github.unclechu.xmonadrc." ⋄)
    , interfaceName = "com.github.unclechu.xmonadrc"
    }


data XlibKeysHackIfaceParams
  = XlibKeysHackIfaceParams
  { objPath ∷ DBus.ObjectPath
  , busName ∷ String → DBus.BusName
  , interfaceName ∷ DBus.InterfaceName
  }

instance Default XlibKeysHackIfaceParams where
  def
    = XlibKeysHackIfaceParams
    { objPath = "/"
    , busName = DBus.busName_ ∘ ("com.github.unclechu.xlib_keys_hack." ⋄)
    , interfaceName = "com.github.unclechu.xlib_keys_hack"
    }
