#! /usr/bin/env runhaskell

-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

-- Clunky converter from TOML to JSON and back.
--
-- This program is not intended to be a proper implementation of the conversion.
-- It does not guarantee that converting from TOML to JSON and then back to TOML
-- would give you the same result. This program does not guarantee idempotency
-- when repeating conversion cycle. Make sure it’s okay for your use-case(s).
--
-- Background:
-- It’s just good enough implementation for my Alacritty configuration
-- generator. With yet another release Alacritty switched from YAML to TOML
-- configuration format. And for my configuration generator I had to convert
-- from TOML to JSON (to be able to use “jq”). I used Remarshal script. But it’s
-- written in Python, and Python is slow to start. I do the conversion each
-- Alacritty run in the wrapper script, and I open new terminal quite often. So
-- the lag before I press a hot key to open a terminal window and the window
-- actaully appears is annoying due to the slow Python invocation. That’s why I
-- came up with this program/script-ish, it’s just doing the conversion between
-- TOML and YAML much much faster, the executable runs instantly. Thus giving me
-- an improvement to the responsiveness of the terminal hot key.
--
-- WARNING! After update to “toml” 2.* version I need to fix some types.
-- For example TOML decoding uses “Text” instead of “String”.
--
-- Standard input is for your TOML/JSON source and the result is printed to
-- standard output.
--
-- Usage example (assuming you’re using the derivation from “default.nix”):
--
--   clunky-toml-json-converter toml2json <<< $'[foo]\nbar.baz = 123'
--   clunky-toml-json-converter json2toml <<< '{"foo":{"bar":{"baz":123}}}'

{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{- HLINT ignore "Redundant <&>" -}

import System.Environment (getArgs)
import Data.Functor ((<&>))
import Data.Bifunctor (bimap)
import Data.String (fromString)
import qualified Toml as TOML
import qualified Data.Aeson as JSON
import qualified Data.Aeson.KeyMap as JSON
import qualified Data.Aeson.Key as JSON
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Text.IO as Text
import qualified Data.Vector as Vector
import qualified Data.Map.Strict as Map

main ∷ IO ()
main =
  getArgs >>= \case
    x | x == ["--help"] || x == ["-h"] →
      putStr usageInfo

    ["toml2json"] →
      (Text.readFile "/dev/stdin" <&> TOML.decode @TOML.Value) >>= \case
        TOML.Failure e → fail $ "Failed to parse TOML:\n" <> unlines e
        TOML.Success _ x → LBS.putStrLn . JSON.encode . toml2json $ x

    ["json2toml"] →
      (LBS.readFile "/dev/stdin" <&> JSON.eitherDecode) >>= \case
        Left e → fail $ "Failed to parse JSON:\n" <> e
        Right x → case json2toml x of
          TOML.Table y → putStr . show . TOML.prettyToml $ y
          y → fail $ unwords
            [ "Failed to encode TOML!"
            , "The top-level value is not a table."
            , "The top-level os your JSON value must be an object."
            , "Instead got:", show y
            ]

    x →
      fail $ "Incorrect argument(s) (" <> show x <> ")!\n" <> usageInfo

usageInfo ∷ String
usageInfo = unlines
  [ "Usage example:"
  , "  clunky-toml-json-converter toml2json <<< $'[foo]\\nbar.baz = 123'"
  , "  clunky-toml-json-converter json2toml <<< '{\"foo\":{\"bar\":{\"baz\":123}}}'"
  ]

toml2json ∷ TOML.Value → JSON.Value
toml2json = \case
  TOML.Integer x → JSON.Number . fromInteger $ x
  TOML.Double x → JSON.Number . fromRational . toRational $ x
  TOML.List x → JSON.Array . Vector.fromList . fmap toml2json $ x
  TOML.Table (TOML.MkTable x) →
    JSON.Object
    . JSON.fromList
    . fmap (bimap JSON.fromText (toml2json . snd))
    . Map.toList
    $ x
  TOML.Bool x → JSON.Bool x
  TOML.Text x → JSON.String x
  -- The date-n-time-related types below are just “showed".
  -- This is “clunky” implementation, it’s not intended to be 100% correct.
  -- So this is a compromise I don’t really worry about for my use cases.
  TOML.TimeOfDay x → JSON.String . fromString . show $ x
  TOML.ZonedTime x → JSON.String . fromString . show $ x
  TOML.LocalTime x → JSON.String . fromString . show $ x
  TOML.Day x → JSON.String . fromString . show $ x

json2toml ∷ JSON.Value → TOML.Value
json2toml = \case
  JSON.Object x →
    TOML.Table
    . TOML.MkTable
    . fmap (((),) . json2toml)
    . JSON.toMapText
    $ x
  JSON.Array x → TOML.List . fmap json2toml . Vector.toList $ x
  JSON.String x → TOML.Text x
  JSON.Number x →
    case toRational x of
      y@(properFraction → (int, frac))
        | frac /= 0 → TOML.Double . fromRational $ y
        | otherwise → TOML.Integer int
  JSON.Bool x → TOML.Bool x
  -- Note that in the TOML model there is no equivalent to “null”.
  -- I could make the top parser only to accept object-like structure
  -- and exclude nulls from arrays and object or something like it.
  -- But this program is not intended to be a correct/proper implementation,
  -- so for the simplicity of the implementation I just convert null into an
  -- empty string here.
  JSON.Null → TOML.Text mempty
