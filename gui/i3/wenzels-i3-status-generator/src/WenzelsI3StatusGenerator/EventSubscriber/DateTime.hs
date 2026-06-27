-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE PackageImports #-}

module WenzelsI3StatusGenerator.EventSubscriber.DateTime
     ( subscribeToDateTimeUpdates
     ) where

import "base" Data.Fixed (Pico)
import "base" Data.Function (fix)
import "time" Data.Time.Clock (UTCTime)

import "time" Data.Time.LocalTime
  ( TimeZone
  , TimeOfDay (todSec)
  , LocalTime (localTimeOfDay)
  , ZonedTime (zonedTimeToLocalTime, zonedTimeZone)
  , getZonedTime
  , zonedTimeToUTC
  )

import "base" Control.Concurrent (threadDelay)

import qualified "async" Control.Concurrent.Async as Async

-- Local imports

import WenzelsI3StatusGenerator.Utils


subscribeToDateTimeUpdates
  ∷ ((UTCTime, TimeZone) → IO ())
  -- ^ Value update callback
  → IO ((UTCTime, TimeZone), Async.Async ())
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


fetchDateAndTime ∷ IO (Pico, UTCTime, TimeZone)
fetchDateAndTime = getZonedTime <&> \zt →
  let
    utc      = zonedTimeToUTC zt
    timeZone = zonedTimeZone  zt
    seconds  = todSec $ localTimeOfDay $ zonedTimeToLocalTime zt
    secsLeft = 60 - seconds -- left to next minute
  in
    (secsLeft, utc, timeZone)
