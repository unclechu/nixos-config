-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE UnicodeSyntax, GHC2024, QuasiQuotes #-}

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

import Control.Concurrent (myThreadId)
import qualified Control.Concurrent.Async as Async
import Control.Exception (SomeException, catch, displayException)
import Control.Monad ((<$!>), void)
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Function ((&))
import Data.Functor ((<&>), ($>))
import Graphics.X11.Xlib (Display, displayString)
import Prelude.Unicode
import qualified System.IO as SysIO
import qualified System.Process as SysProc
import Text.InterpolatedString.QM (qms)


-- * Operators

(•) ∷ (α → β) → (β → γ) → α → γ; (•) = flip (∘); {-# INLINE (•) #-}; infixl 9 •
(⋄) ∷ Semigroup α ⇒ α → α → α;   (⋄) = (<>);     {-# INLINE (⋄) #-}; infixr 6 ⋄

(<&!>) ∷ Monad φ ⇒ φ α → (α → β) → φ β
(<&!>) = flip (<$!>)
{-# INLINE (<&!>) #-}
infixl 1 <&!>


-- * Helper functions

-- | Print a "ByteString" to stdout and flush it immediatelly
echo ∷ LBS.ByteString → IO ()
echo s = LBS.putStrLn s >> SysIO.hFlush SysIO.stdout


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
  devNull <- SysIO.openFile "/dev/null" SysIO.ReadWriteMode
  void $ SysProc.createProcess (SysProc.proc cmd args)
    { SysProc.std_in = SysProc.UseHandle devNull
    , SysProc.std_out = SysProc.UseHandle devNull
    , SysProc.std_err = SysProc.UseHandle devNull
    , SysProc.new_session = True
    }


-- | Spawn fire-and-forget thread and report exception if it occurs inside it
fireAndForget ∷ IO () → IO ()
fireAndForget m =
  void ∘ Async.async $ catch m $ \e → do
    threadId ← myThreadId
    SysIO.hPutStrLn SysIO.stderr [qms|
      Fire-and-forget thread ({threadId}) failed with exception:
      {displayException @SomeException e}
    |]
