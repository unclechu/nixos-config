-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE UnicodeSyntax, GHC2024 #-}

module WenzelsI3StatusGenerator.Utils.Aeson
     ( withFieldNamer
     ) where

import Data.Aeson (defaultOptions)
import Data.Aeson.Types (Options (fieldLabelModifier), camelTo2)
import WenzelsI3StatusGenerator.Utils ((∘))


withFieldNamer ∷ (String → String) → Options
withFieldNamer f = defaultOptions { fieldLabelModifier = f ∘ camelTo2 '_' }
