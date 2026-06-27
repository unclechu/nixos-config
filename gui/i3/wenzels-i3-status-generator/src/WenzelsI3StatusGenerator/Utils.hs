-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UnicodeSyntax #-}

module WenzelsI3StatusGenerator.Utils
     ( (•), (⋄), (<&!>)
     , module Prelude.Unicode
     , module Data.Function
     , module Data.Functor
     , echo
     , getDisplayName
     , spawnProc
     , fireAndForget
     ) where

import Prelude hiding (putStrLn)
import "base-unicode-symbols" Prelude.Unicode

import "base" Data.Function ((&))
import "base" Data.Functor ((<&>), ($>))
import "bytestring" Data.ByteString.Lazy.Char8 (ByteString, putStrLn)
import "qm-interpolated-string" Text.InterpolatedString.QM (qms)

import "base" Control.Concurrent (myThreadId)
import "base" Control.Exception (SomeException, catch, displayException)
import "base" Control.Monad ((<$!>), void)
import qualified "async" Control.Concurrent.Async as Async

import "base" System.IO
  ( IOMode (ReadWriteMode)
  , stdout
  , stderr
  , hPutStrLn
  , hFlush
  , openFile
  )

import "process" System.Process
  ( CreateProcess (std_in, std_out, std_err, new_session)
  , StdStream (UseHandle)
  , proc
  , createProcess
  )

import "X11" Graphics.X11.Xlib (Display, displayString)


-- * Operators

(•) ∷ (α → β) → (β → γ) → α → γ; (•) = flip (∘); {-# INLINE (•) #-}; infixl 9 •
(⋄) ∷ Semigroup α ⇒ α → α → α;   (⋄) = (<>);     {-# INLINE (⋄) #-}; infixr 6 ⋄

(<&!>) ∷ Monad φ ⇒ φ α → (α → β) → φ β
(<&!>) = flip (<$!>)
{-# INLINE (<&!>) #-}
infixl 1 <&!>


-- * Helper functions

-- | Print a "ByteString" to stdout and flush it immediatelly
echo ∷ ByteString → IO ()
echo s = putStrLn s >> hFlush stdout


-- | Get a display name (printed number) with special symbols replaced with @_@
--   underscore so that you can use it as a part of some service names
--
-- Can be useful for defining DBus IPC scoped to a particular X11 session.
getDisplayName ∷ Display → String
getDisplayName dpy = go where
  go = f <$> displayString dpy

  f ':' = '_'
  f '.' = '_'
  f  x  =  x


-- | Spawn a process in fire-and-forget mode
spawnProc ∷ FilePath → [String] → IO ()
spawnProc cmd args = do
  devNull <- openFile "/dev/null" ReadWriteMode
  void $ createProcess (proc cmd args)
    { std_in = UseHandle devNull
    , std_out = UseHandle devNull
    , std_err = UseHandle devNull
    , new_session = True
    }


-- | Spawn fire-and-forget thread and report exception if it occurs inside it
fireAndForget ∷ IO () → IO ()
fireAndForget m =
  void ∘ Async.async $ catch m $ \e → do
    threadId ← myThreadId
    hPutStrLn stderr [qms|
      Fire-and-forget thread ({threadId}) failed with exception:
      {displayException @SomeException e}
    |]
