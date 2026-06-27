-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

module WenzelsI3StatusGenerator.EventSubscriber.Battery
     ( UPowerBatteryState (..)
     , subscribeToBatteryChargeUpdates
     ) where

import "base" Data.Word (Word8)
import "base" Data.Either (isRight)
import "base" Data.Foldable (find)
import "base" Data.String (fromString)
import "qm-interpolated-string" Text.InterpolatedString.QM (qms)
import qualified "containers" Data.Map.Strict as Map
import qualified "attoparsec" Data.Attoparsec.ByteString.Char8 as Parsec

import "base" Control.Monad (void)

import qualified "dbus" DBus
import qualified "dbus" DBus.Internal.Types as DBusInternal
import qualified "dbus" DBus.Client

-- Local imports

import WenzelsI3StatusGenerator.Utils


subscribeToBatteryChargeUpdates
  ∷ ((Maybe Double, Maybe UPowerBatteryState) → IO ())
  -- ^ Battery charge info update callback
  → IO (Maybe ((Double, UPowerBatteryState), IO ()))
  -- ^ Initial value and unsubscribe function
subscribeToBatteryChargeUpdates updateCallback = do
  client ← DBus.Client.connectSystem

  !batteryObjPath ←
    DBus.Client.call_
      client
      ( DBus.methodCall
          "/org/freedesktop/UPower"
          "org.freedesktop.UPower"
          "EnumerateDevices"
      ) { DBus.methodCallDestination = Just "org.freedesktop.UPower" }

      <&!> \reply → let
             objPaths =
               [ y
               | Just x ← DBus.fromVariant <$> DBus.methodReturnBody reply
               , y ← (x ∷ [DBus.ObjectPath])
               ]

             -- …/battery_BAT0
             parser = void
               $ Parsec.manyTill' Parsec.anyChar "/battery_BAT"
               <* (Parsec.decimal ∷ Parsec.Parser Word8)
               <* Parsec.endOfInput

             batteryObjPath
               = find
               ( isRight
               ∘ Parsec.parseOnly parser
               ∘ fromString
               ∘ DBus.formatObjectPath
               ) objPaths ∷ Maybe DBus.ObjectPath

             in batteryObjPath

  case batteryObjPath of
    Nothing → Nothing <$ DBus.Client.disconnect client
    Just !x → do
      unsubscriber ←
        catchUpdate client x
          <&> DBus.Client.removeMatch client
          <&> (>> DBus.Client.disconnect client)
      chargeLeft ← getPropCall client x ∘ show $ Percentage
      chargeState ← getPropCall client x ∘ show $ State
      pure $ Just ((chargeLeft, chargeState), unsubscriber)

  where
    -- Method call to gets a property of a battery device
    getPropCall
      ∷ DBus.IsVariant α
      ⇒ DBus.Client.Client
      → DBus.ObjectPath
      → UPowerPropName
      → IO α
    getPropCall client batteryObjPath propName = go where
      go = DBus.Client.call_ client propCall >>= DBus.methodReturnBody • \case
        [x] → case DBus.fromVariant x >>= DBus.fromVariant of
          Nothing → fail [qms| Unexpected UPower reply: {x} |]
          Just y  → pure y

        x → fail [qms| Unexpected UPower reply: {x} |]

      propCall =
        (DBus.methodCall batteryObjPath "org.freedesktop.DBus.Properties" "Get")
          { DBus.methodCallDestination = Just "org.freedesktop.UPower"
          , DBus.methodCallBody =
              DBus.toVariant <$> ["org.freedesktop.UPower.Device", propName]
          }

    catchUpdate
      ∷ DBus.Client.Client
      → DBus.ObjectPath
      → IO DBus.Client.SignalHandler
    catchUpdate client betteryObj = go where
      go = DBus.Client.addMatch client rule $ handler ∘ DBus.signalBody
      propsToWatch = show <$> [minBound .. maxBound ∷ WatchedProp]

      handler
        [ DBus.fromVariant → Just ("org.freedesktop.UPower.Device" ∷ String)

        , DBus.fromVariant →
            Just props@(Map.keys → any (∈ propsToWatch) → True)
              ∷ Maybe (Map.Map String DBus.Variant)

        , DBus.fromVariant → Just (_ ∷ [DBus.Variant])
        ]
        = updateCallback
            ( DBus.fromVariant =<< Map.lookup (show Percentage) props
            , DBus.fromVariant =<< Map.lookup (show State)      props
            )

      -- Ignore not watched properties
      handler
        [ DBus.fromVariant → Just ("org.freedesktop.UPower.Device" ∷ String)

        , DBus.fromVariant →
            Just (Map.keys → any (∈ propsToWatch) → False)
              ∷ Maybe (Map.Map String DBus.Variant)

        , DBus.fromVariant → Just (_ ∷ [DBus.Variant])
        ]
        = pure ()

      handler x =
        fail [qms| Unexpected UPower property changed signal body: {x} |]

      rule
        = DBus.Client.matchAny
        { DBus.Client.matchPath = Just betteryObj
        , DBus.Client.matchInterface = Just "org.freedesktop.DBus.Properties"
        , DBus.Client.matchMember = Just "PropertiesChanged"
        }


-- * Types

data WatchedProp = Percentage | State
  deriving (Eq, Show, Enum, Bounded)


type UPowerPropName = String


data UPowerBatteryState
  = Unknown
  | Charging
  | Discharging
  | Empty
  | FullyCharged
  | PendingCharge
  | PendingDischarge
  deriving (Eq, Show)

instance Enum UPowerBatteryState where
  toEnum = \case
    1 → Charging
    2 → Discharging
    3 → Empty
    4 → FullyCharged
    5 → PendingCharge
    6 → PendingDischarge
    _ → Unknown

  fromEnum = \case
    Unknown          → 0
    Charging         → 1
    Discharging      → 2
    Empty            → 3
    FullyCharged     → 4
    PendingCharge    → 5
    PendingDischarge → 6

instance DBus.IsVariant UPowerBatteryState where
  toVariant
    = DBusInternal.Variant
    ∘ DBusInternal.ValueAtom
    ∘ DBusInternal.AtomWord32
    ∘ fromIntegral
    ∘ fromEnum

  fromVariant
    ( DBusInternal.Variant
    ( DBusInternal.ValueAtom
    ( DBusInternal.AtomWord32 x ))) = Just ∘ toEnum ∘ fromIntegral $ x
  fromVariant _ = Nothing
