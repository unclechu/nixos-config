-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE UnicodeSyntax, GHC2024 #-}
{-# LANGUAGE ViewPatterns #-}

-- | Serializing the state of the application into a string
module WenzelsI3StatusGenerator.Render
     ( render
     ) where

import Data.Aeson (ToJSON (..), genericToJSON, encode)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Default (Default (def))
import Data.Time.Format (FormatTime, formatTime, defaultTimeLocale)
import Data.Time.LocalTime (utcToZonedTime)
import Data.Tuple (swap)
import Data.Word (Word8)
import GHC.Generics (Generic)
import WenzelsI3StatusGenerator.EventSubscriber.Battery (UPowerBatteryState (..))
import WenzelsI3StatusGenerator.Handler.AppState.Types (State (..))
import qualified WenzelsI3StatusGenerator.Indicators as Indicators
import WenzelsI3StatusGenerator.Layout (Layout, colorOfLayout)
import WenzelsI3StatusGenerator.Utils ((↔), (<&>), (≡), (∘), (≥))
import WenzelsI3StatusGenerator.Utils.Aeson (withFieldNamer)


-- | Render given state to a string (encoded JSON value)
--
-- This item should be a part of an open streaming list printed to stdout.
-- Adding commas and open of the list is out of the responsibilities of this
-- function. It should be handled somewhere else.
render ∷ State → ByteString
render s
  = (encode ∷ [Unit] → ByteString)
  $ maybe mempty (\x → [windowTitleView x, _separate]) (windowTitle s)
  ↔
  [ numLockView s
  , capsLockView s
  , alternativeView s
  , _separate
  ]
  ↔ kbdLayoutView s
  ↔ [ _separate, dateAndTimeView s ]
  ↔ maybe mempty (\x → [_separate, batteryView x]) (battery s)


-- * Units

numLockView ∷ State → Unit
numLockView s
  = def
  { fullText = Indicators.showNumLock isOn
  , color = Just $ Indicators.colorOfNumLock isOn
  , name = Just "numlock"
  }
  where
    isOn = numLock s


capsLockView ∷ State → Unit
capsLockView s
  = def
  { fullText = Indicators.showCapsLock isOn
  , color = Just $ Indicators.colorOfCapsLock isOn
  , name = Just "capslock"
  }
  where
    isOn = capsLock s


alternativeView ∷ State → Unit
alternativeView s
  = def
  { fullText =
      either (\n → "%UNKNOWN:" ↔ show n ↔ "%") Prelude.id $
        Indicators.showAlternativeState alternativeState

  , color =
      either (const Nothing) Just $
        Indicators.colorOfAlternativeState alternativeState

  , name = Just "alternative"
  }
  where
    alternativeState = alternative s


-- Layout names are just hardcoded, they may be not in this exact order.
kbdLayoutView ∷ State → [Unit]
kbdLayoutView s = go where
  f nameSuffix fullText (Just → color) =
    def { name = Just ("kbdlayout-" ↔ nameSuffix), fullText, color }

  go = case kbdLayout s of
    Nothing → pure $ f "UNDEFINED" "%UNDEFINED%" "#eeeeee"
    Just (Left n) →
      pure $ f "UNKNOWN" ("%UNKNOWN:" ↔ show n ↔ "%") "#eeeeee"
    Just (Right layout) →
      [minBound .. maxBound ∷ Layout] <&> \x →
        f (show x) (show x) $
          if x ≡ layout then colorOfLayout layout else "#666666"


dateAndTimeView ∷ State → Unit
dateAndTimeView s = go where
  go = maybe def { fullText = "…" } (set ∘ render') $ lastTime s
  render' = renderDate ∘ uncurry utcToZonedTime ∘ swap
  set x  = def { fullText = x, name = Just "datentime" }

  renderDate ∷ FormatTime t ⇒ t → String
  renderDate = formatTime defaultTimeLocale "%A %-d %B %H:%M"


batteryView ∷ (Double, UPowerBatteryState) → Unit
batteryView (chargeLeft, batteryState) = go where
  go = def
    { -- Rounding because floating point is always zero
      fullText = icon batteryState ↔ show (round chargeLeft ∷ Word8) ↔ "%"

    , name = Just "battery"

    , color =
        Just $ case batteryState of
          Charging → connectedToAdapterColor
          FullyCharged → connectedToAdapterColor
          _ | chargeLeft ≥ 80 → "#00ff00"
            | chargeLeft < 20 → "#ff0000"
            | otherwise → "#ffff00"

    }

  connectedToAdapterColor = "#00ffff"
  dischargingIcon = "🔋"
  chargingIcon = "⚡"

  icon = \case
    Charging → chargingIcon
    FullyCharged → chargingIcon
    _ → dischargingIcon


windowTitleView ∷ String → Unit
windowTitleView x = def
  { fullText = x
  , name = Just "window-title"
  }


_separate ∷ Unit
_separate = def { fullText = "/", color = Just "#666666" }
-- separateAfter x = x { separator = Just True, separatorBlockWidth = Just 20 }


-- * Types

data Unit
  = Unit
  { fullText ∷ String
  , shortText ∷ Maybe String
  , color ∷ Maybe String
  , background ∷ Maybe String
  , border ∷ Maybe String
  , minWidth ∷ Maybe Word
  , align ∷ Maybe String
  , name ∷ Maybe String
  , _instance ∷ Maybe String
  , urgent ∷ Maybe Bool
  , separator ∷ Maybe Bool
  , separatorBlockWidth ∷ Maybe Word
  , markup ∷ Maybe String
  }
  deriving (Show, Eq, Generic)

instance Default Unit where
  def
    = Unit
    { fullText = ""
    , shortText = Nothing
    , color = Just "#999999"
    , background = Nothing
    , border = Nothing
    , minWidth = Nothing
    , align = Nothing
    , name = Nothing
    , _instance = Nothing
    , urgent = Nothing
    , separator = Just False
    , separatorBlockWidth = Nothing
    , markup              = Just "none"
    }

instance ToJSON Unit where
  toJSON = genericToJSON $ withFieldNamer f
    where f ('_' : xs) = xs; f x = x
