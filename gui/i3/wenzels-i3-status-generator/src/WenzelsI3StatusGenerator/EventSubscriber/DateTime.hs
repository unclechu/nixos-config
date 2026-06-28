-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE UnicodeSyntax, GHC2024 #-}

module WenzelsI3StatusGenerator.EventSubscriber.DateTime
     ( subscribeToDateTimeUpdates
     ) where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import Data.Fixed (Pico)
import Data.Function (fix)
import Data.Time.Clock (UTCTime)
import qualified Data.Time.LocalTime as Time
import WenzelsI3StatusGenerator.Utils ((∘), (×), (<&>))


subscribeToDateTimeUpdates
  ∷ ((UTCTime, Time.TimeZone) → IO ())
  -- ^ Value update callback
  → IO ((UTCTime, Time.TimeZone), Async.Async ())
  -- ^ Initial value and the thread handle
subscribeToDateTimeUpdates updateCallback = do
  (initialSecondsLeftToNextMinute, initialUtc, initialTimeZone) ←
    fetchDateAndTime

  threadHandle ←
    Async.async
      ∘ ($ initialSecondsLeftToNextMinute)
      ∘ fix
      $ \again secondsToWait → do
          threadDelay ∘ ceiling $ secondsToWait × 1000 × 1000
          (secondsLeftToNextMinute, utc, timeZone) ← fetchDateAndTime
          updateCallback (utc, timeZone)
          again secondsLeftToNextMinute

  pure ((initialUtc, initialTimeZone), threadHandle)


fetchDateAndTime ∷ IO (Pico, UTCTime, Time.TimeZone)
fetchDateAndTime = Time.getZonedTime <&> \zt →
  let
    utc      = Time.zonedTimeToUTC zt
    timeZone = Time.zonedTimeZone  zt
    seconds  = Time.todSec $ Time.localTimeOfDay $ Time.zonedTimeToLocalTime zt
    secsLeft = 60 - seconds -- left to next minute
  in
    (secsLeft, utc, timeZone)
