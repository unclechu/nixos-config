{-# LANGUAGE UnicodeSyntax, BangPatterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Main (main) where

import Prelude hiding (fail)

import Data.Functor ((<&>))
import qualified Text.Read as Read (Read (readPrec), lift, choice, readEither)
import qualified Text.ParserCombinators.ReadP as Read (string)

import Control.Monad (join, filterM)
import Control.Monad.Fail (fail)

import System.Environment (getEnv)
import System.FilePath ((</>))
import System.Directory (listDirectory)
import System.Posix.Files (fileExist, getFileStatus, isDirectory)
import System.Process.Typed (proc, runProcess_)

main ∷ IO ()
main = do
  !user ← getUser

  let grantAccess f = do
        putStrLn $
          "Granting ACL read access to '" ◇ f ◇ "' input device for '" ◇
          show user ◇ "' user…"

        runProcess_ $ proc "setfacl" ["-m", "u:" ◇ show user ◇ ":r", "--", f]

  traverse (\dir → fmap (dir </>) <$> listDirectory dir) dirs
    >>= filterM fileExist ∘ join
    >>= filterM (fmap (not ∘ isDirectory) ∘ getFileStatus)
    >>= mapM_ grantAccess

getUser ∷ IO User
getUser = go where
  go = getEnv "USER" >>= \u → either (failure u) pure (Read.readEither u)
  failure u = fail ∘ mappend ("Failed to recognize user '" ◇ u ◇ "': ")

data User = Wenzel deriving (Enum, Bounded)

instance Show User where
  show Wenzel = "wenzel"

instance Read User where
  readPrec
    = Read.choice
    $ [minBound .. maxBound] <&> \u → Read.lift $ u <$ Read.string (show u)

dirs ∷ [FilePath]
dirs = ("/dev/input/by-" ◇) <$> ["id", "path"]

(∘) = (.); (◇) = (<>)
