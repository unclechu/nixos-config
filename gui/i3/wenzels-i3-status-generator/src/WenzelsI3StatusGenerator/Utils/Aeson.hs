-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax #-}

module WenzelsI3StatusGenerator.Utils.Aeson
     ( withFieldNamer
     ) where

import "base-unicode-symbols" Prelude.Unicode

import "aeson" Data.Aeson (defaultOptions)
import "aeson" Data.Aeson.Types (Options (fieldLabelModifier), camelTo2)


withFieldNamer ∷ (String → String) → Options
withFieldNamer f = defaultOptions { fieldLabelModifier = f ∘ camelTo2 '_' }
