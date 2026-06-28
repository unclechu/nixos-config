-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE UnicodeSyntax, GHC2024 #-}

module WenzelsI3StatusGenerator.X
     ( initThreads
     , fakeKeyEvent
     ) where

import Control.Monad (forM, unless)
import Foreign.C.Types (CULong (CULong), CInt (CInt))
import qualified Graphics.X11.Xlib as Xlib
import Graphics.X11.Xlib.Misc (keysymToKeycode)
import WenzelsI3StatusGenerator.Utils ((≠), fireAndForget, (∘), (≢))


foreign import ccall unsafe "X11/extensions/XTest.h XTestFakeKeyEvent"
  xFakeKeyEvent ∷ Xlib.Display → Xlib.KeyCode → Bool → CULong → IO Xlib.Status

foreign import ccall unsafe "X11/Xlib.h XInitThreads"
  xInitThreads ∷ IO Xlib.Status


initThreads ∷ IO ()
initThreads =
  xInitThreads >>= \x → unless (x ≠ 0) (fail "XInitThreads call has failed")


-- | Trigger fake key event in fire-and-forget mode
fakeKeyEvent
  ∷ [(Xlib.KeySym, Bool)]
  -- ^ A list of key state changes to evaluate sequentially
  --   ("Bool" means whether key is pressed or released)
  → IO ()
fakeKeyEvent keySyms = fireAndForget $ do
  dpy ← Xlib.openDisplay ""
  keyCodes dpy >>= mapM_ (uncurry $ trig dpy)
  Xlib.closeDisplay dpy

  where
    keyCodes dpy = forM keySyms $ \(k, s) → (, s) ∘ test <$> keysymToKeycode dpy k
    test = (\(x, True) → x) ∘ (\x → (x, x ≢ 0))
    trig dpy k s = xFakeKeyEvent dpy k s 0 >> Xlib.sync dpy False
