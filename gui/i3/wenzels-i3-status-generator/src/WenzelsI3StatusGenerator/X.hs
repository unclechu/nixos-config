-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnicodeSyntax #-}

module WenzelsI3StatusGenerator.X
     ( initThreads
     , fakeKeyEvent
     ) where

import "base" Control.Monad (forM, unless)

import "base" Foreign.C.Types (CULong (CULong), CInt (CInt))

import "X11" Graphics.X11.Xlib.Misc (keysymToKeycode)

import "X11" Graphics.X11.Xlib
  ( Display (Display)
  , KeyCode
  , Status
  , KeySym
  , openDisplay
  , closeDisplay
  , sync
  )

 -- Local imports

import WenzelsI3StatusGenerator.Utils


foreign import ccall unsafe "X11/extensions/XTest.h XTestFakeKeyEvent"
  xFakeKeyEvent ∷ Display → KeyCode → Bool → CULong → IO Status

foreign import ccall unsafe "X11/Xlib.h XInitThreads"
  xInitThreads ∷ IO Status


initThreads ∷ IO ()
initThreads =
  xInitThreads >>= \x → unless (x ≠ 0) (fail "XInitThreads call has failed")


-- | Trigger fake key event in fire-and-forget mode
fakeKeyEvent
  ∷ [(KeySym, Bool)]
  -- ^ A list of key state changes to evaluate sequentially
  --   ("Bool" means whether key is pressed or released)
  → IO ()
fakeKeyEvent keySyms = fireAndForget $ do
  dpy ← openDisplay ""
  keyCodes dpy >>= mapM_ (uncurry $ trig dpy)
  closeDisplay dpy

  where
    keyCodes dpy = forM keySyms $ \(k, s) → (, s) ∘ test <$> keysymToKeycode dpy k
    test = (\(x, True) → x) ∘ (\x → (x, x ≢ 0))
    trig dpy k s = xFakeKeyEvent dpy k s 0 >> sync dpy False
