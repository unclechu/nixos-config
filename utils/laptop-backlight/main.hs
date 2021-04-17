-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE UnicodeSyntax, LambdaCase, ViewPatterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (main) where

import Prelude hiding (fail, max)

import Data.Char (toLower)
import Data.String (fromString)
import qualified Data.Attoparsec.ByteString.Char8 as P

import Control.Monad ((>=>))
import Control.Monad.Fail (fail)
import Control.Applicative ((<|>))

import System.FilePath ((</>))
import System.Environment (getArgs)
import System.Exit (die)

main ∷ IO ()
main = do
  max ← readSomeFile Maximum

  getArgs >>= \case
    ["get-max"] → print max
    ["get"] → readSomeFile Current >>= print

    ["set", parseNumber → Right n] | n ≤ max →
      writeFile (getPath Current) (show n)

    args → die $ "Unexpected arguments: " ⋄ show args

data File = Maximum | Current deriving Show

getPath ∷ File → FilePath
getPath =
  ("/sys/class/backlight/intel_backlight" </>) ∘ \case
    Maximum → "max_brightness"
    Current → "brightness"

readSomeFile ∷ File → IO Word
readSomeFile file = go $ getPath file where
  go = readFile >=> parseNumber • either (fail ∘ failMsg) pure

  failMsg
    = mappend
    $ "Failed to parse " ⋄ fmap toLower (show file) ⋄ " brightness value: "

parseNumber ∷ String → Either String Word
parseNumber = go where
  go = P.parseOnly parser ∘ fromString
  parser = P.decimal <* (() <$ P.char '\n' <|> pure ()) <* P.endOfInput

(∘) = (.); (•) = flip (.); (⋄) = (<>); (≤) = (<=)
