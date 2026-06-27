-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | Some helper functions for printing indicators/mods
module WenzelsI3StatusGenerator.Indicators
     ( showNumLock
     , colorOfNumLock

     , showCapsLock
     , colorOfCapsLock

     , showAlternativeState
     , colorOfAlternativeState
     ) where

import "base" Data.Bool (bool)
import "base" Data.Word (Word8)


showNumLock ∷ Bool → String
showNumLock = const "num"

colorOfNumLock ∷ Bool → String
colorOfNumLock = bool "#999999" "#eeeeee"


showCapsLock ∷ Bool → String
showCapsLock = bool "caps" "CAPS"

colorOfCapsLock ∷ Bool → String
colorOfCapsLock = bool "#999999" "#ff9900"


showAlternativeState ∷ Maybe (Word8, Bool) → Either Word8 String
showAlternativeState = go where
  text = bool "hax" "HAX"

  go = \case
    Nothing     → Right $ text False
    Just (1, p) → Right $ text p
    Just (2, p) → Right $ text p
    Just (n, _) → Left n

colorOfAlternativeState ∷ Maybe (Word8, Bool) → Either Word8 String
colorOfAlternativeState = \case
  Nothing     → Right "#999999"
  Just (1, _) → Right "#ffff00"
  Just (2, _) → Right "#00ffff"
  Just (n, _) → Left n
