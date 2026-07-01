-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE UnicodeSyntax, GHC2024 #-}

-- | Dzen2 notification window spawning functions
module WenzelsI3StatusGenerator.Dzen
     ( dzen
     ) where

import WenzelsI3StatusGenerator.Utils (fireAndForget, spawnProc)


dzen ∷ String → String → IO ()
dzen text fgColor = fireAndForget $ spawnProc "dzen-box" [text, fgColor]
{-# INLINE dzen #-}
