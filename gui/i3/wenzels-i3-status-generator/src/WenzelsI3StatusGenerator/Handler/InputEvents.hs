-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE UnicodeSyntax, GHC2024, ViewPatterns #-}

-- | Module responsible of handling events coming from stdin
module WenzelsI3StatusGenerator.Handler.InputEvents
     ( HandleClickEventInterface (..)
     , handleClickEvent
     ) where

import Control.Monad (join)
import Data.Function (fix)
import qualified Graphics.X11.Types as XTypes
import WenzelsI3StatusGenerator.EventSubscriber.InputEvents (ClickEvent (..))
import WenzelsI3StatusGenerator.Layout (Layout)
import WenzelsI3StatusGenerator.Utils (spawnProc, (≡), (∘), (&))
import WenzelsI3StatusGenerator.X (fakeKeyEvent)


-- | Handle single click event
handleClickEvent ∷ HandleClickEventInterface → ClickEvent → IO ()
handleClickEvent iface (\x → name (x ∷ ClickEvent) → Just name') = case name' of
  "numlock" → fakeKeyEvent $ (XTypes.xK_Num_Lock,) <$> [False, True, False]
  "capslock" → fakeKeyEvent $ (XTypes.xK_Caps_Lock,) <$> [False, True, False]
  "datentime" → spawnProc "gnome-calendar" []
  "alternative" → alternativeModeClickHandler iface

  ['k','b','d','l','a','y','o','u','t','-',a,b] →
    let
      enum = [minBound .. maxBound ∷ Layout]
      next = go where
        go = foldr reducer [] [False, True, False]
        reducer s acc = (XTypes.xK_Shift_L, s) : (XTypes.xK_Shift_R, s) : acc
      switchTo curLayout toLayout = go where
        go = join $ replicate n next
        n = fix (\f x@(l:ls) → if l ≡ curLayout then x else f ls) (cycle enum)
          & fix (\f i (l:ls) → if l ≡ toLayout then i else f (succ i) ls) (0 ∷ Int)
    in do
      layout ← getCurrentKbdLayout iface
      fakeKeyEvent ∘ maybe next (uncurry switchTo) $ (,)
        <$> layout
        <*> foldl (\acc l → if show l ≡ [a,b] then Just l else acc) Nothing enum

  _ → pure ()

handleClickEvent _ _ = pure ()


-- * Types

data HandleClickEventInterface
  = HandleClickEventInterface
  { alternativeModeClickHandler ∷ IO ()
  , getCurrentKbdLayout ∷ IO (Maybe Layout)
  }
