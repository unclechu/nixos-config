-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE UnicodeSyntax, GHC2024 #-}

-- | Module responsible of handling application state
module WenzelsI3StatusGenerator.Handler.AppState.Types
     ( State (..)
     ) where

import Data.Default (Default (def))
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (TimeZone)
import Data.Word (Word8)
import WenzelsI3StatusGenerator.EventSubscriber.Battery (UPowerBatteryState)
import WenzelsI3StatusGenerator.Layout (Layout)


data State
  = State
  { numLock ∷ Bool
  , capsLock ∷ Bool

  , alternative ∷ Maybe (Word8, Bool)
  -- ^ Alternative mode state
  --   (@Bool@ indicates whether alternative mode is turned on permanently)

  , kbdLayout ∷ Maybe (Either Word8 Layout)
  -- ^ Keyboard layout (@Left@ contains unknown layout number
  --   if parsing of "Layout" has failed)

  , lastTime ∷ Maybe (UTCTime, TimeZone)
  -- ^ Current date-time
  --   (the reason why don't just store "ZonedTime" here
  --   is that it doesn't have "Eq" instance,
  --   see https://github.com/haskell/time/issues/50 for details)

  , battery ∷ Maybe (Double, UPowerBatteryState)
  -- ^ Battery charge state

  , windowTitle ∷ Maybe String
  -- ^ The title of currenlty focused window
  }
  deriving (Show, Eq)

instance Default State where
  def
    = State
    { numLock = False
    , capsLock = False
    , alternative = Nothing
    , kbdLayout = Nothing
    , lastTime = Nothing
    , battery = Nothing
    , windowTitle = Nothing
    }
