-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns #-}

-- | Listening to input events
module WenzelsI3StatusGenerator.EventSubscriber.InputEvents
     ( ClickEvent (..)
     , subscribeToClickEvents
     ) where

import "base" GHC.Generics (Generic)
import Prelude hiding (getLine)

import "aeson" Data.Aeson (FromJSON (..), eitherDecodeStrict, genericParseJSON)
import "base" Data.Word (Word8)
import "bytestring" Data.ByteString.Char8 (getLine, uncons)
import "qm-interpolated-string" Text.InterpolatedString.QM (qm)

import "base" Control.Monad (forever)
import qualified "async" Control.Concurrent.Async as Async

-- Local imports

import WenzelsI3StatusGenerator.Utils
import WenzelsI3StatusGenerator.Utils.Aeson (withFieldNamer)


-- | Reading click events from i3-bar
subscribeToClickEvents ∷ (ClickEvent → IO ()) → IO (Async.Async ())
subscribeToClickEvents eventCallback = Async.async $ do
  getLine >>= \case
    "[" → pure () -- Opening of the list items stream
    x → fail $ "Incorrect opening of JSON list: " ⋄ show x

  -- First one (without comma separator)
  getLine >>= parseItem >>= eventCallback

  forever @IO @() @() $ do
    getLine >>= uncons • \case
      Nothing →
        fail [qm| Failed to parse JSON: Unexpected end of input |]
      Just (',', x) →
        parseItem x >>= eventCallback
      Just x →
        fail [qm| Failed to parse JSON: Missing comma separator (input: {x}) |]

  where
    parseItem = eitherDecodeStrict • \case
      Left e → fail [qm| Failed to parse JSON: {show e} |]
      Right x → pure x


-- * Types

data ClickEvent
  = ClickEvent
  { name ∷ Maybe String
  , _instance ∷ Maybe String
  , button ∷ Word8
  , _x ∷ Int
  , _y ∷ Int
  }
  deriving (Show, Eq, Generic)

instance FromJSON ClickEvent where
  parseJSON = genericParseJSON $ withFieldNamer f
    where f ('_' : xs) = xs; f x = x
