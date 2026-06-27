-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedRecordDot #-}

-- | A collection of IPC signals
module WenzelsI3StatusGenerator.IPC
     ( ipcSwitchAlternativeModeSignal
     ) where

import "base" Data.Word (Word32)
import "data-default" Data.Default (def)

import qualified "dbus" DBus

-- Local imports

import WenzelsI3StatusGenerator.Utils

import WenzelsI3StatusGenerator.EventSubscriber.IPC
  ( XmonadrcIfaceParams (..)
  , XlibKeysHackIfaceParams (..)
  )


-- | Switch alternative mode level to a given value
ipcSwitchAlternativeModeSignal
  ∷ (∀a. (String → a) → a)
  -- ^ Function that provides current display marker
  → Word32
  -- ^ Alternative mode level value (@0@ turns it off)
  → DBus.Signal
ipcSwitchAlternativeModeSignal withDisplayMarker newAlternativeState = signal where
  objPath' = (def ∷ XlibKeysHackIfaceParams).objPath
  ifaceName = (def ∷ XlibKeysHackIfaceParams).interfaceName
  memberName = "switch_alternative_mode"

  signal
    = (DBus.signal objPath' ifaceName memberName)
    { DBus.signalSender =
        Just ∘ withDisplayMarker $ (def ∷ XmonadrcIfaceParams).busName
    , DBus.signalDestination =
        Just ∘ withDisplayMarker $ (def ∷ XlibKeysHackIfaceParams).busName
    , DBus.signalBody = [DBus.toVariant newAlternativeState]
    }
