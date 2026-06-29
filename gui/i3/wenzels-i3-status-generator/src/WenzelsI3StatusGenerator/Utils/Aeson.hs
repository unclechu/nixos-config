-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE UnicodeSyntax, GHC2024 #-}

module WenzelsI3StatusGenerator.Utils.Aeson
     ( withFieldNamer
     ) where

import Data.Aeson (defaultOptions)
import qualified Data.Aeson.Types as AesonT
import WenzelsI3StatusGenerator.Utils ((∘))


withFieldNamer ∷ (String → String) → AesonT.Options
withFieldNamer f = defaultOptions
  { AesonT.fieldLabelModifier = f ∘ AesonT.camelTo2 '_'
  -- Reduce the amount of transmitted JSON by removing all the `null` fields
  , AesonT.omitNothingFields = True
  }
