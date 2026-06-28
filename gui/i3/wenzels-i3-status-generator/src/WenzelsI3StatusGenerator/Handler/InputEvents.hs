-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE UnicodeSyntax, GHC2024, ViewPatterns #-}

-- | Module responsible of handling events coming from stdin
module WenzelsI3StatusGenerator.Handler.InputEvents
     ( HandleClickEventInterface (..)
     , handleClickEvent
     ) where

import Control.Monad (join)
import Data.Maybe (fromMaybe)
import qualified Graphics.X11.Types as XTypes
import WenzelsI3StatusGenerator.EventSubscriber.InputEvents (ClickEvent (..))
import WenzelsI3StatusGenerator.Layout (Layout)
import WenzelsI3StatusGenerator.Utils (spawnProc, (≡))
import WenzelsI3StatusGenerator.X (fakeKeyEvent)


-- | Handle single click event
handleClickEvent ∷ HandleClickEventInterface → ClickEvent → IO ()
handleClickEvent iface (\x → name (x ∷ ClickEvent) → Just name') = case name' of
  "numlock" → fakeKeyEvent $ (XTypes.xK_Num_Lock,) <$> [False, True, False]
  "capslock" → fakeKeyEvent $ (XTypes.xK_Caps_Lock,) <$> [False, True, False]
  "datentime" → spawnProc "gnome-calendar" []
  "alternative" → alternativeModeClickHandler iface

  -- For example: @kbdlayout-US@ or @kbdlayout-FI@
  ['k','b','d','l','a','y','o','u','t','-',a,b] →
    let
      enum = [minBound .. maxBound ∷ Layout]
      totalLayoutsCount = length enum

      targetLayoutCode ∷ String
      targetLayoutCode = [a,b]

      -- Find specified "Layout" by its code (e.g. `US`)
      targetLayout ∷ Maybe Layout
      targetLayout =
        let reducer acc l = if show l ≡ targetLayoutCode then Just l else acc
        in foldl reducer Nothing enum

      -- | A key event sequence to switch to next layout
      --
      -- First reset the key state to released (in case it was pressed at the
      -- time or triggering this function) then regular press+release sequence.
      nextLayout ∷ [(XTypes.KeySym, Bool)]
      nextLayout = go where
        go = foldr reducer [] [False, True, False]
        reducer s acc = (XTypes.xK_Shift_L, s) : (XTypes.xK_Shift_R, s) : acc

      -- | Get a key event sequence to switch to @toLayout@ (destination layout)
      --   from @curLayout@ (current layout)
      switchTo ∷ Layout → Layout → [(XTypes.KeySym, Bool)]
      switchTo curLayout toLayout =
        let
          -- | How many times to press layout switch key chord to switch to the
          --   destination layout
          switchCount =
            (fromEnum toLayout - fromEnum curLayout) `mod` totalLayoutsCount
        in
          join $ replicate switchCount nextLayout
    in do
      layout ← getCurrentKbdLayout iface
      -- If layout is not found (@Nothing@, current layout and/or target layout)
      -- just press switch-to-next-layout key chord
      fakeKeyEvent $ fromMaybe nextLayout (switchTo <$> layout <*> targetLayout)

  _ → pure ()

handleClickEvent _ _ = pure ()


-- * Types

data HandleClickEventInterface
  = HandleClickEventInterface
  { alternativeModeClickHandler ∷ IO ()
  , getCurrentKbdLayout ∷ IO (Maybe Layout)
  }
