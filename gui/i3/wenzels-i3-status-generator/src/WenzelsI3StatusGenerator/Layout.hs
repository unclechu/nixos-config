-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE UnicodeSyntax, GHC2024 #-}

-- | "Layout" data-type and related functions
module WenzelsI3StatusGenerator.Layout
     ( Layout (..)
     , numToLayout
     , colorOfLayout
     ) where

import Data.List (find)
import WenzelsI3StatusGenerator.Utils ((•), (≡), (&))


-- | Layouts definition
--
-- The order defines here the mapping to the system layout number.
-- The name of the constructor is used for printing the layout name.
data Layout = US | RU | FI deriving (Eq, Show, Enum, Bounded)


numToLayout ∷ ∀ α. (Num α, Enum α, Bounded α, Eq α) ⇒ α → Maybe Layout
numToLayout n = zip [0..] [minBound..maxBound] & find (fst • (≡ n)) & fmap snd


colorOfLayout ∷ Layout → String
colorOfLayout = \case US → "#ff0000"; RU → "#00ff00"; FI → "#0000ff"
