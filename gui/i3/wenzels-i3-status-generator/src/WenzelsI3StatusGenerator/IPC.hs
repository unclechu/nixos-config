-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE UnicodeSyntax, GHC2024, OverloadedRecordDot, OverloadedStrings #-}

-- | A collection of IPC signals
module WenzelsI3StatusGenerator.IPC
     ( ipcSwitchAlternativeModeSignal
     ) where

import Data.Default (def)
import Data.Word (Word32)
import qualified DBus
import qualified WenzelsI3StatusGenerator.EventSubscriber.IPC as EventIPC
import WenzelsI3StatusGenerator.Utils ((∘))


-- | Switch alternative mode level to a given value
ipcSwitchAlternativeModeSignal
  ∷ (∀a. (String → a) → a)
  -- ^ Function that provides current display marker
  → Word32
  -- ^ Alternative mode level value (@0@ turns it off)
  → DBus.Signal
ipcSwitchAlternativeModeSignal withDisplayMarker newAlternativeState = signal where
  objPath' = (def ∷ EventIPC.XlibKeysHackIfaceParams).objPath
  ifaceName = (def ∷ EventIPC.XlibKeysHackIfaceParams).interfaceName
  memberName = "switch_alternative_mode"

  signal
    = (DBus.signal objPath' ifaceName memberName)
    { DBus.signalSender =
        Just ∘ withDisplayMarker $ (def ∷ EventIPC.XmonadrcIfaceParams).busName
    , DBus.signalDestination =
        Just ∘ withDisplayMarker $ (def ∷ EventIPC.XlibKeysHackIfaceParams).busName
    , DBus.signalBody = [DBus.toVariant newAlternativeState]
    }
