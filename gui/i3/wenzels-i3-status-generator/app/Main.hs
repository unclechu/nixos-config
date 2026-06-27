-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE UnicodeSyntax #-}

module Main (main) where

import WenzelsI3StatusGenerator.App (runApp)

main ∷ IO ()
main = runApp
