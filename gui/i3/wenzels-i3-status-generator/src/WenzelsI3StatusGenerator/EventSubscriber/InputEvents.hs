-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE UnicodeSyntax, GHC2024, QuasiQuotes, OverloadedStrings #-}

-- | Listening to input events
module WenzelsI3StatusGenerator.EventSubscriber.InputEvents
     ( ClickEvent (..)
     , subscribeToClickEvents
     ) where

import qualified Control.Concurrent.Async as Async
import Control.Monad (forever)
import Data.Aeson (FromJSON (..), eitherDecodeStrict, genericParseJSON)
import qualified Data.ByteString.Char8 as BS
import Data.Word (Word8)
import GHC.Generics (Generic)
import Prelude hiding (getLine)
import Text.InterpolatedString.QM (qm)
import WenzelsI3StatusGenerator.Utils ((•), (⋄))
import WenzelsI3StatusGenerator.Utils.Aeson (withFieldNamer)


-- | Reading click events from i3-bar
subscribeToClickEvents ∷ (ClickEvent → IO ()) → IO (Async.Async ())
subscribeToClickEvents eventCallback = Async.async $ do
  BS.getLine >>= \case
    "[" → pure () -- Opening of the list items stream
    x → fail $ "Incorrect opening of JSON list: " ⋄ show x

  -- First one (without comma separator)
  BS.getLine >>= parseItem >>= eventCallback

  forever @IO @() @() $ do
    BS.getLine >>= BS.uncons • \case
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
