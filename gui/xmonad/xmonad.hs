-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

-- Useful readings:
--
-- * https://tronche.com/gui/x/icccm/sec-4.html
-- * https://specifications.freedesktop.org/wm-spec/wm-spec-1.3.html

-- TODO: Prevent focus grabbing by other windows that are not explicitly allowed to do so.
--       Look how it’s done in my old config: https://github.com/unclechu/xmonadrc

-- TODO: Try out dynamic workspaces.
--       XMonad.Actions.DynamicWorkspaces
--       XMonad.Actions.DynamicWorkspaceOrder
--       XMonad.Actions.DynamicWorkspaceGroups

-- TODO: Try out Polybar. There is no bar used yet in this config.
--       https://polybar.github.io
--       https://github.com/polybar/polybar
--       https://gvolpe.com/blog/xmonad-polybar-nixos/

-- TODO: Find out if “free layouts” like in i3wm are possible in XMonad.
--       Every tile can be either one window or nest one of the layouts:
--       1. Vertical/horizontal split
--       2. Tabs/Stacks
--       And it’s recursive. Any nested tile can nest other tiles, etc.

-- TODO: Add some multiple windows killing key bindings. See: XMonad.Actions.WithAll

-- TODO: Check out https://github.com/Procrat/xmonad-contexts and see if it can be useful

-- TODO: Fix resizing of the floating windows when worksapces are moved between
--       screens with different resolution.
--       Try to patch XMonad.Operations.windows.
--       As pointed out by geekosaur from #xmonad:libera.chat here is the place to change:
--       https://github.com/xmonad/xmonad/blob/1aac661/src/XMonad/Operations.hs#L197-L202

-- TODO: Come up with a i3wm-like solution where workspaces are assigned to
--       particular screens. So workspaces would have some kind of memory
--       effect what screen they were on last time. And if you try to go to
--       that workspace it opens on a screen where it was on last time.
--       The key bindings for moving current workspace between screens
--       can work as a way to bind a workspace to another screen.
--
-- TODO: Compe up with a way to restart/reload XMonad but without running “autostart-setup”
--       (as an optional feature). Sometimes I just want to reload the “config” (new Xmonad build)
--       without running everything that “autostart-setup” involves.

{-# OPTIONS_GHC -Wall -Wno-partial-type-signatures #-}

{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import XMonad (XConfig (..), X, (.|.), (|||), (-->), (=?))
import qualified XMonad
import qualified XMonad.Hooks.Modal as HModal
import qualified XMonad.Hooks.EwmhDesktops as EwmhDesktops
import qualified Data.Map.Strict as Map
import Text.InterpolatedString.QM (qms, qns)
import qualified Graphics.X11.ExtraTypes.XF86 as XF86
import qualified XMonad.StackSet as W
import qualified XMonad.Hooks.Modal as Modal
import Control.Monad (replicateM_)
import qualified XMonad.Layout.ResizableTile as ResizableTile
import Data.Foldable (traverse_)
import qualified XMonad.Layout.Tabbed as Tabbed
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Actions.NoBorders (toggleBorder)
import qualified XMonad.Hooks.ManageDocks as ManageDocks
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import qualified XMonad.Layout.Spacing as Spacing
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import qualified XMonad.Layout.GridVariants as GridVariants
import qualified XMonad.Layout.ResizableThreeColumns as ResizableThreeColumns
import qualified XMonad.Layout.Renamed as Renamed
import XMonad.Actions.CopyWindow (copyToAll, killAllOtherCopies)
import XMonad.Util.WindowProperties (getProp32)
import qualified XMonad.Actions.TagWindows as TagWindows
import qualified XMonad.Actions.FloatKeys as FloatKeys
import qualified XMonad.Actions.FlexibleResize as FlexibleResize
import qualified XMonad.Actions.CycleWS as CycleWS
import Data.List (partition)
import Data.Maybe (listToMaybe)
import XMonad.Hooks.TaffybarPagerHints (pagerHints)
import qualified Data.List.NonEmpty as NE

main ∷ IO ()
main = XMonad.xmonad . configCustomizations $ XMonad.def

-- * Commands

-- Calling terminals
cmdTerminalDark, cmdTerminalLight ∷ String
cmdTerminalDark = "alacritty-jetbrains-font-dark"
cmdTerminalLight = "alacritty-jetbrains-font-light"

-- Some SIGsomething sending commands
cmdExterminate, cmdPause, cmdPauseRecursive, cmdResume, cmdResumeRecursive ∷ String
cmdExterminate = "kill -KILL -- $(( $(xdotool getactivewindow getwindowpid) ))"
cmdPause = "kill -STOP -- $(( $(xdotool getactivewindow getwindowpid) ))"
cmdPauseRecursive = [qns|
  (
    n=0;
    rec() {
      set -o errexit || return;
      set -o nounset;
      if (( ++n > 10 )); then return 1; fi;
      kill -STOP -- $(($1));
      pgrep -P $(($1)) | while read -r x; do rec $((x)); done;
    }
    && rec $(( $(xdotool getactivewindow getwindowpid) ))
  )
|]
cmdResume = "kill -CONT -- $(( $(xdotool getactivewindow getwindowpid) ))"
cmdResumeRecursive = [qns|
  (
    n=0;
    rec() {
      set -o errexit || return;
      set -o nounset;
      if (( ++n > 10 )); then return 1; fi;
      kill -CONT -- $(($1));
      pgrep -P $(($1)) | while read -r x; do rec $((x)); done;
    }
    && rec $(( $(xdotool getactivewindow getwindowpid) ))
  )
|]

-- App/command GUI runners

cmdRunDark, cmdRunLight, cmdDRunDark, cmdDRunLight ∷ String
cmdRunDark = rofiCmd RofiModeRun RofiThemeDark
cmdRunLight = rofiCmd RofiModeRun RofiThemeLight
cmdDRunDark = rofiCmd RofiModeDRun RofiThemeDark
cmdDRunLight = rofiCmd RofiModeDRun RofiThemeLight

cmdSelectWindowDark, cmdSelectWindowLight ∷ String
cmdSelectWindowDark = rofiCmd RofiModeWindow RofiThemeDark
cmdSelectWindowLight = rofiCmd RofiModeWindow RofiThemeLight

cmdCursorToDisplay ∷ DisplayN → String
cmdCursorToDisplay dn = [qms| cursor-to-display -d {displayNToNum dn ∷ Word} |]

cmdCursorToDisplayAndPlaceAt ∷ DisplayN → String
cmdCursorToDisplayAndPlaceAt dn = [qms| {cmdCursorToDisplay dn} && sleep .1 && place-cursor-at |]

data DisplayN = D1 | D2 | D3 | D4 deriving (Eq, Show, Enum, Bounded, Ord)
displayNToNum ∷ Num a ⇒ DisplayN → a
displayNToNum = \case D1 → 1 ; D2 → 2 ; D3 → 3 ; D4 → 4

-- Missing from porting my i3wm config:
--
-- - New workspace
-- - New temporary workspace
--
-- - Outputs navigation
--
--   I uses “i3-msg” to get the info about the outputs.
--   Try call “xrandr” and parse the output to get this info.
--   Or maybe use some native XMonad way for obtaining the screen info.
--
--   Example:
--     i3-msg -t get_outputs | jq -r '[.[] | select(.active)] | sort_by(.rect.x, .rect.y) | .[0].name'
--     i3-msg -t get_outputs | jq -r '[.[] | select(.active)] | sort_by(.rect.x, .rect.y) | .[1].name'

-- * XMonad configuration

-- | All the configuration customizations together
configCustomizations ∷ XConfig _layoutA → XConfig _layoutB
configCustomizations
  -- Real fullscreen instead of bounding the fullscreen to the window dimentions.
  -- But I personally prefer to keep windows inside their tiles even when they are fullscreened.
  -- N.B. Note that this one must come above “ewmh”.
  -- ewmhFullscreen

  = ManageDocks.docks
  . EwmhDesktops.ewmh
  . pagerHints
  . withKeysModes
  . myConfig

myConfig ∷ XConfig _layoutA → XConfig _layoutB
myConfig x = x
  { modMask = XMonad.mod4Mask -- Super/Meta/Windows key
  , workspaces = show <$> [1 .. 10 ∷ Word]

  , terminal = "alacritty" -- I don’t really use this value, I have some custom key bindings instead
  , normalBorderColor = "#222222"
  , focusedBorderColor = "#ff0000"
  , focusFollowsMouse = False
  , clickJustFocuses = False

  -- Key/button bindings:

  , keys = defaultModeKeys
  , mouseBindings = myMouseBindings

  -- Hooks:

  -- This startup script is provided by my NixOS configuration
  , startupHook = XMonad.spawn "sleep 1s ; autostart-setup"

  , layoutHook = myLayout
  , manageHook = myManageHook
  }

tabsLayoutName, fullscreenLayoutName ∷ String
tabsLayoutName = "Tabs"
fullscreenLayoutName = "Fullscreen"

myLayout ∷ _layout
myLayout = go where
  go =
    -- “avoidStruts” to avoid overlapping with the bars
    ManageDocks.avoidStruts (layoutSpacing tiles ||| tabbed ||| full)
    ||| fullscreen

  -- | Fullscreen-ish layout (no borders, no bars, just one window)
  fullscreen = named fullscreenLayoutName $ noBorders XMonad.Full

  -- | One window takes whole screen except it respects the struts/bars and window border
  full = named "Full" XMonad.Full

  -- | Layouts where multiple windows are showing at the same time
  tiles
    = named "V-split" tall
    ||| named "H-split" (XMonad.Mirror tall)
    ||| grids

  -- | Two-column (or two-row when mirrored) layout
  tall = ResizableTile.ResizableTall 1 delta ratio []

  -- | Tree-column (or three-row) or grid-like layouts
  grids
    = named "Grid" (GridVariants.SplitGrid GridVariants.L 2 3 (2 / 3) (16 / 10) delta)
    ||| named "Three Columns" (ResizableThreeColumns.ResizableThreeColMid 1 delta ratio [])

  -- | Regular tabs layout where only one window shows at a time
  tabbed = named tabsLayoutName $ Tabbed.tabbed Tabbed.shrinkText myTabTheme

  named name = Renamed.renamed [Renamed.Replace name]

  delta = 1 / 100 -- Resize step
  ratio = 1 / 2 -- Default ratio between the master and the slave windows

  myTabTheme :: Tabbed.Theme
  myTabTheme = XMonad.def
    { Tabbed.activeColor = "#285577"
    , Tabbed.activeTextColor = "#ffffff"
    , Tabbed.activeBorderColor = "#4c7899"

    , Tabbed.inactiveColor = "#222222"
    , Tabbed.inactiveTextColor = "#888888"
    , Tabbed.inactiveBorderColor = "#333333"

    , Tabbed.urgentColor = "#900000"
    , Tabbed.urgentTextColor = "#ffffff"
    , Tabbed.urgentBorderColor = "#2f343a"

    , Tabbed.fontName = "xft:Hack:size=10"
    , Tabbed.decoHeight = 20
    }

-- | Add some space gap to a given layout
layoutSpacing
  ∷ layout a
  → ModifiedLayout Renamed.Rename (ModifiedLayout Spacing.Spacing layout) a
layoutSpacing
  = Renamed.renamed [Renamed.CutWordsLeft 1] -- Remove “Spacing” prefix
  . Spacing.spacingRaw False (Spacing.Border 0 size 0 size) True (Spacing.Border size 0 size 0) True
  where size = 10

-- ** Mouse bindings

-- | Mouse button mapping set
type MouseButtons = Map.Map (XMonad.KeyMask, XMonad.Button) (XMonad.Window -> X ())

-- | Mouse bindings: default actions bound to mouse events
myMouseBindings :: XConfig layout -> MouseButtons
myMouseBindings XConfig { XMonad.modMask = m } = Map.fromList
  -- %! Set the window to floating mode and move by dragging
  [ ( (m, XMonad.button1)
    , \window -> do
        XMonad.focus window
        XMonad.mouseMoveWindow window
        XMonad.windows W.shiftMaster
    )

  -- %! Raise the window to the top of the stack
  , ((m, XMonad.button2), XMonad.windows . (W.shiftMaster .) . W.focusWindow)

  -- %! Set the window to floating mode and resize by dragging
  , ( (m, XMonad.button3)
    , \window -> do
        XMonad.focus window
        -- XMonad.mouseResizeWindow window
        FlexibleResize.mouseResizeWindow window
        XMonad.windows W.shiftMaster
    )
  ]

-- ** Key binding modes

-- | Key mapping set
type Keys = Map.Map (XMonad.KeyMask, XMonad.KeySym) (X ())

a, s ∷ XMonad.KeyMask
a = XMonad.mod1Mask -- “a” for “Alt”
s = XMonad.shiftMask

hjklKeys, mirroredHjklKeys, arrowKeys, displayKeys ∷ [XMonad.KeySym]
hjklKeys = [XMonad.xK_h, XMonad.xK_j, XMonad.xK_k, XMonad.xK_l]
-- Like HJLK but for the left hand.
-- They are not really mirrored, the left-to-right order is the same.
mirroredHjklKeys = [XMonad.xK_a, XMonad.xK_s, XMonad.xK_d, XMonad.xK_f]
arrowKeys = [XMonad.xK_Left, XMonad.xK_Down, XMonad.xK_Up, XMonad.xK_Right]
displayKeys = [XMonad.xK_u, XMonad.xK_i, XMonad.xK_o, XMonad.xK_p]

-- | Window focus movement keys
--
-- In my i3wm config the HJKL focus is actually moving to the picked direction.
-- In XMonad there’s only “focusUp” and “focusDown” for the directions.
-- But even this doesn’t mean “up” and “down” but rather “previous” and “next”.
-- TODO: Find out if it’s possible in XMonad to move the focus directionally
--       depending on window position.
windowFocusKeys ∷ XMonad.KeyMask → Keys
windowFocusKeys m = Map.fromList go where
  operations = [W.focusMaster, W.focusDown, W.focusUp, focusLast]
  go =
    [ ((m, key), XMonad.windows operation)
    | keys ← [hjklKeys, arrowKeys]
    , (key, operation) ← zip keys operations
    ]

-- | Focused window movement keys
windowMoveKeys ∷ XMonad.KeyMask → Keys
windowMoveKeys m = Map.fromList go where
  operations = [W.swapMaster, W.swapDown, W.swapUp]
  go =
    [ ((m, key), XMonad.windows operation)
    | keys ← [hjklKeys, arrowKeys]
    , (key, operation) ← zip keys operations
    ]

-- | Keys in relation to navigating and manipulating floating windows
floatingWindowsKeys ∷ XMonad.KeyMask → Keys
floatingWindowsKeys m = Map.fromList
  -- Toggle tiling / floating
  [ ((m .|. a, XMonad.xK_space), XMonad.withFocused toggleFloatWindow)
  , ((m, XMonad.xK_space), toggleFloatingTiledFocus)
  ]

type WorkspaceLabel = String

-- | Walking between workspaces and moving windows between workspaces
navigationBetweenWorkspacesKeys ∷ [WorkspaceLabel] → (XMonad.KeyMask, XMonad.KeyMask) → Keys
navigationBetweenWorkspacesKeys ws (switchMask, moveMask) = Map.fromList
  [ ((mask, key), XMonad.windows $ operation workspace)
  | (workspace, key) <- zip ws $ [XMonad.xK_1 .. XMonad.xK_9] <> [XMonad.xK_0]
  , (mask, operation) <-
      [ (switchMask, W.view) -- %! Switch to workspace
      , (moveMask, W.shift) -- %! Move focused window to workspace
      ]
  ]

-- | Jumping to different displays and moving windows to displays keys
navigationBetweenDisplaysKeys ∷ (XMonad.KeyMask, XMonad.KeyMask, XMonad.KeyMask) → Keys
navigationBetweenDisplaysKeys (jumpMask, jumpMouseCursorMask, moveToMask) = Map.fromList
  [ ((mask, key), operation display)
  | (key, display) ← zip displayKeys [minBound .. maxBound ∷ DisplayN]
  , (mask, operation) ←
      [ ( jumpMask
        , forWsOnDisplay $ \d n → do
            XMonad.windows (W.view n)
            XMonad.spawn (cmdCursorToDisplay d)
        )

      , ( jumpMouseCursorMask
        , forWsOnDisplay $ \d n → do
            XMonad.windows (W.view n)
            XMonad.spawn (cmdCursorToDisplayAndPlaceAt d)
        )

      , (moveToMask, forWsOnDisplay $ \_d n → XMonad.windows (W.shift n))
      ]
  ]
  where
    -- | Convert "DisplayN" to a screen index
    --
    -- Screen number starts with 0 while "DisplayN" starts with 1.
    -- TODO: Order screens by their geometry and position (like in my i3wm config).
    displayNToScreenIndex = pred . displayNToNum

    -- | For workspace that is on the specified display right now
    forWsOnDisplay ∷ (DisplayN → XMonad.WorkspaceId → X ()) → DisplayN → X ()
    forWsOnDisplay f n = XMonad.screenWorkspace (displayNToScreenIndex n) >>= traverse_ (f n)

defaultModeKeys ∷ XConfig XMonad.Layout → Keys
defaultModeKeys
  XConfig
    { XMonad.modMask = m
    , XMonad.workspaces = ws
    , XMonad.layoutHook = layout
    } =
  windowFocusKeys m
  <> windowMoveKeys (m .|. a)
  <> floatingWindowsKeys m
  <> navigationBetweenWorkspacesKeys ws (m, m .|. a)
  <> navigationBetweenDisplaysKeys (m, m .|. a, m .|. s)
  <> workspaceControlKeys
  <> layoutControlKeys
  <> modeSwitchingKeys
  <> commandSpawningKeys
  <> mediaKeys
  where
    -- | Keys for controlling workspaces
    workspaceControlKeys = Map.fromList
      -- Next/previous workspace switching keys
      [ ((m, XMonad.xK_z), CycleWS.moveTo CycleWS.Prev CycleWS.hiddenWS)
      , ((m, XMonad.xK_x), CycleWS.moveTo CycleWS.Next CycleWS.hiddenWS)
      , ((m .|. a, XMonad.xK_z), CycleWS.shiftTo CycleWS.Prev CycleWS.hiddenWS)
      , ((m .|. a, XMonad.xK_x), CycleWS.shiftTo CycleWS.Next CycleWS.hiddenWS)

      -- Moving workspaces between displays
      , ((m, XMonad.xK_comma), CycleWS.swapPrevScreen >> CycleWS.prevScreen)
      , ((m, XMonad.xK_period), CycleWS.swapNextScreen >> CycleWS.nextScreen)
      , ((m .|. a, XMonad.xK_comma), CycleWS.swapPrevScreen)
      , ((m .|. a, XMonad.xK_period), CycleWS.swapNextScreen)

      -- Find an empty workspace that’s currently not on any of the screens and open it.
      -- TODO: When implementing dynamic workspaces this should create a new workspace
      --       in case it runs out of pre-existing workspaces
      --       (unless I’ll implement those dynamic workspaces with no pre-existing
      --       workspaces, except few matching the amount of screens, in this case
      --       it should just create a new workspace).
      , ( (m, XMonad.xK_b)
        , CycleWS.moveTo CycleWS.Next (CycleWS.emptyWS CycleWS.:&: CycleWS.hiddenWS)
        )
      -- Move the window to an empty hidden workspace and jump to the window on that new workspace
      , ( (m .|. a, XMonad.xK_b)
        , XMonad.withFocused $ \w → do
            CycleWS.shiftTo CycleWS.Next (CycleWS.emptyWS CycleWS.:&: CycleWS.hiddenWS)
            XMonad.focus w -- Jump to the window on the new workspace (switch to that workspace)
        )
      ]

    -- | Keys for controlling current layout or switch between layouts
    layoutControlKeys = Map.fromList
      -- TODO: Port from my i3wm config.
      -- TODO: Try out the i3wm-like layout.
      -- bindsym $m+c split v
      -- bindsym $m+v split h

      -- TODO: Try to port this from my i3wm config.
      --       I’ve seen in the past there are some extensions that simulate those i3-like layout splits.
      --   bindsym $m+s layout stacking
      --   bindsym $m+w layout tabbed
      --   bindsym $m+e layout toggle split
      -- Until I figure out how to do i3-like layouting
      -- I use these keys for regular XMonad layout rotation.
      -- , ((m, XMonad.xK_s), _)
      [ ((m, XMonad.xK_w), XMonad.setLayout layout) -- Reset layout state
      , ((m, XMonad.xK_e), XMonad.sendMessage XMonad.NextLayout)
      -- Switch to tabs
      , ((m .|. a, XMonad.xK_w), XMonad.sendMessage (XMonad.JumpToLayout tabsLayoutName))

      -- Fullscreen the window (jump to no border fullscreen layout)
      , ( (m, XMonad.xK_f)
        -- Tile the window first in case it’s floating so it can be fullscreened-ish
        , XMonad.withFocused (XMonad.windows . W.sink)
          >> XMonad.sendMessage (XMonad.JumpToLayout fullscreenLayoutName)
        )

      -- TODO: Try to port this from my i3wm config.
      --       It only makes sense.
      -- # focus the parent container
      -- bindsym $m+a focus parent
      -- # focus the child container
      -- bindsym $m+q focus child

      -- XMonad master windows amount manipulation
      , ((m, XMonad.xK_c), XMonad.sendMessage $ XMonad.IncMasterN 1)
      , ((m .|. a, XMonad.xK_c), XMonad.sendMessage $ XMonad.IncMasterN (-1))
      , ((m .|. s, XMonad.xK_c), XMonad.refresh)

      -- Toggling struts for the layout, and border on/off for the window
      , ((m, XMonad.xK_v), XMonad.sendMessage ManageDocks.ToggleStruts)
      , ((m .|. a, XMonad.xK_v), XMonad.withFocused toggleBorder)
      ]

    -- | Keys for switching to different modes
    modeSwitchingKeys = Map.fromList
      [ ((m, XMonad.xK_d), Modal.setMode doKeysModeLabel)
      , ((m, XMonad.xK_g), Modal.setMode workspaceKeysModeLabel)
      , ((m, XMonad.xK_r), Modal.setMode $ resizeKeysModeToLabel ResizeKeysModeNormal)
      , ((m, XMonad.xK_t), Modal.setMode $ positioningKeysModeToLabel PositioningKeysModeNormal)
      , ((m .|. a, XMonad.xK_m), Modal.setMode mouseCursorKeysModeLabel)
      ]

    commandSpawningKeys = Map.fromList
      -- Terminals
      [ ((m, XMonad.xK_Return), XMonad.spawn cmdTerminalDark)
      , ((m .|. a, XMonad.xK_Return), XMonad.spawn cmdTerminalLight)

      -- App/command GUI runners
      , ((m, XMonad.xK_semicolon), XMonad.spawn cmdRunDark)
      , ((m .|. a, XMonad.xK_semicolon), XMonad.spawn cmdRunLight)
      , ((m .|. s, XMonad.xK_semicolon), XMonad.spawn cmdDRunDark)
      , ((m .|. s .|. a, XMonad.xK_semicolon), XMonad.spawn cmdDRunLight)

      -- Window selection GUI
      , ((m, XMonad.xK_slash), XMonad.spawn cmdSelectWindowDark)
      , ((m .|. a, XMonad.xK_slash), XMonad.spawn cmdSelectWindowLight)

      -- Clipboard management tool
      , ((m, XMonad.xK_apostrophe), XMonad.spawn "gpaste-gui")
      , ((m .|. a, XMonad.xK_apostrophe), XMonad.spawn "gpaste-gui -m=choose")

      -- Inverting window colors
      , ((m, XMonad.xK_n), XMonad.spawn "timeout 2s invert-window-colors")

      -- Mouse cursor jump helper
      , ((m, XMonad.xK_m), XMonad.spawn "place-cursor-at")
      ]

    mediaKeys = Map.fromList
      -- Making screenshots
      [ ((0, XMonad.xK_Print), XMonad.spawn "gnome-screenshot")
      , ((m, XMonad.xK_Print), XMonad.spawn "gnome-screenshot -w")
      -- TODO: Test it ↓ This bind was with “--release” flag in my i3wm config.
      , ((0, XMonad.xK_Pause), XMonad.spawn "gnome-screenshot -a")
      , ((m, XMonad.xK_Pause), XMonad.spawn "gnome-screenshot -ia")

      -- Calculator
      , ((0, XF86.xF86XK_Calculator), XMonad.spawn "gnome-calculator")

      -- Audio player control
      , ((m, XF86.xF86XK_AudioPlay), XMonad.spawn "audacious --play")
      , ((0, XF86.xF86XK_AudioPlay), XMonad.spawn "audacious --play-pause")
      , ((0, XF86.xF86XK_AudioPrev), XMonad.spawn "audacious --rew")
      , ((0, XF86.xF86XK_AudioNext), XMonad.spawn "audacious --fwd")
      , ((0, XF86.xF86XK_AudioStop), XMonad.spawn "audacious --stop")

      -- Audio control
      , ((m, XF86.xF86XK_AudioMute), XMonad.spawn "pamng reset")
      , ((0, XF86.xF86XK_AudioMute), XMonad.spawn "pamng mute")
      , ((0, XF86.xF86XK_AudioLowerVolume), XMonad.spawn "pamng dec")
      , ((s, XF86.xF86XK_AudioLowerVolume), XMonad.spawn "pamng dec '5.0dB'")
      , ((0, XF86.xF86XK_AudioRaiseVolume), XMonad.spawn "pamng inc")
      , ((s, XF86.xF86XK_AudioRaiseVolume), XMonad.spawn "pamng inc '5.0dB'")

      -- Adjust screen backlight level
      , ((0, XF86.xF86XK_MonBrightnessDown), XMonad.spawn "screen-backlight -1%")
      , ((s, XF86.xF86XK_MonBrightnessDown), XMonad.spawn "screen-backlight -5%")
      , ((0, XF86.xF86XK_MonBrightnessUp), XMonad.spawn "screen-backlight +1%")
      , ((s, XF86.xF86XK_MonBrightnessUp), XMonad.spawn "screen-backlight +5%")
      ]

data ResizeKeysMode
  = ResizeKeysModeNormal
  | ResizeKeysModeBigSteps
  deriving (Eq, Show, Enum, Bounded)

resizeKeysModeToLabel ∷ ResizeKeysMode → String
resizeKeysModeToLabel ResizeKeysModeNormal = "Resize"
resizeKeysModeToLabel ResizeKeysModeBigSteps = "Resize (big steps)"

resizeKeysModeToStepRepeats ∷ Num a ⇒ ResizeKeysMode → a
resizeKeysModeToStepRepeats ResizeKeysModeNormal = 1
resizeKeysModeToStepRepeats ResizeKeysModeBigSteps = 5

resizeKeysMode ∷ ResizeKeysMode → Modal.Mode
resizeKeysMode mode = go where
  label = resizeKeysModeToLabel mode
  go = Modal.mode label $ \XConfig { XMonad.modMask = m } →
    let
      repeatN = resizeKeysModeToStepRepeats mode

      operations =
        [ f (XMonad.Shrink, (LT, EQ))
        , f (ResizableTile.MirrorShrink, (EQ, LT))
        , f (ResizableTile.MirrorExpand, (EQ, GT))
        , f (XMonad.Expand, (GT, EQ))
        ]
        where
          vectorIsh = \case EQ → const 0 ; LT → negate ; GT → id

          f (msg, (x, y)) (repeats, step) =
            XMonad.withFocused $ \window → do
              isFloating ← isWindowFloating window
              if not isFloating
                then replicateM_ repeats (XMonad.sendMessage msg)
                else
                  FloatKeys.keysResizeWindow (vectorIsh x step, vectorIsh y step) (0, 0) window

      growShrink mask (repeats, step) = Map.fromList
        -- TODO: Refactor (repeatN * 2) which is supposed to be the same as in “Positioning” mode.
        --       “resizeKeysModeToStepRepeats” can probably return 2 independent numbers.
        [ ((mask, key), operation (repeats, step * (repeatN * 2)))
        | directionKeys ← [hjklKeys, arrowKeys]
        , (key, operation) ← zip directionKeys operations
        ]

      colsRowsAmountKeys = Map.fromList
        [ ((0, key), XMonad.sendMessage operation)
        | (key, operation) ←
            zip
              mirroredHjklKeys
              [ GridVariants.IncMasterCols (-1)
              , GridVariants.IncMasterRows (-1)
              , GridVariants.IncMasterRows 1
              , GridVariants.IncMasterCols 1
              ]
        ]
    in
    Map.fromList
    [ ((0, XMonad.xK_Escape), Modal.exitMode)

    , ( (m, XMonad.xK_r)
      -- Toggle between “normal” and “big steps” of this mode
      , Modal.setMode . resizeKeysModeToLabel $ case mode of
          ResizeKeysModeNormal → ResizeKeysModeBigSteps
          ResizeKeysModeBigSteps → ResizeKeysModeNormal
      )

    , ( (m, XMonad.xK_t)
      -- Switching to “positioning” mode
      , Modal.setMode . positioningKeysModeToLabel $ case mode of
          ResizeKeysModeNormal → PositioningKeysModeNormal
          ResizeKeysModeBigSteps → PositioningKeysModeBigSteps
      )

    , ((0, XMonad.xK_z), XMonad.sendMessage $ XMonad.IncMasterN (-1))
    , ((0, XMonad.xK_x), XMonad.sendMessage $ XMonad.IncMasterN 1)
    ]
    <> modeLeavingKeys (floatingWindowsKeys m)
    <> windowFocusKeys m
    <> growShrink 0 (repeatN, 1)
    <> growShrink a (2 * repeatN, 5)
    <> growShrink s (3 * repeatN, 10)
    <> colsRowsAmountKeys

data PositioningKeysMode
  = PositioningKeysModeNormal
  | PositioningKeysModeBigSteps
  deriving (Eq, Show, Enum, Bounded)

positioningKeysModeToLabel ∷ PositioningKeysMode → String
positioningKeysModeToLabel PositioningKeysModeNormal = "Positioning"
positioningKeysModeToLabel PositioningKeysModeBigSteps = "Positioning (big steps)"

positioningKeysModeToStepRepeats ∷ Num a ⇒ PositioningKeysMode → a
positioningKeysModeToStepRepeats PositioningKeysModeNormal = 1
positioningKeysModeToStepRepeats PositioningKeysModeBigSteps = 10

positioningKeysMode ∷ PositioningKeysMode → Modal.Mode
positioningKeysMode mode = go where
  label = positioningKeysModeToLabel mode
  go = Modal.mode label $ \XConfig { XMonad.modMask = m } →
    let
      repeatN = positioningKeysModeToStepRepeats mode

      moveKeys = Map.fromList
        [ ((mask, key), XMonad.withFocused . FloatKeys.keysMoveWindow $ vectorIsh change step)
        | keys ← [hjklKeys, arrowKeys]
        , (key, change) ← zip keys [(LT, EQ), (EQ, GT), (EQ, LT), (GT, EQ)]
        , (mask, step) ← [(0, 1), (a, 5), (s, 10)]
        ]
        where
          vectorIsh (x, y) step = (f x step, f y step)
          f o n = (case o of EQ → const 0 ; LT → negate; GT → id) (n * repeatN)
    in
    Map.fromList
    [ ((0, XMonad.xK_Escape), Modal.exitMode)

    , ( (m, XMonad.xK_r)
      -- Switching to “positioning” mode
      , Modal.setMode . resizeKeysModeToLabel $ case mode of
          PositioningKeysModeNormal → ResizeKeysModeNormal
          PositioningKeysModeBigSteps → ResizeKeysModeBigSteps
      )

    , ( (m, XMonad.xK_t)
      -- Toggle between “normal” and “big steps” of this mode
      , Modal.setMode . positioningKeysModeToLabel $ case mode of
          PositioningKeysModeNormal → PositioningKeysModeBigSteps
          PositioningKeysModeBigSteps → PositioningKeysModeNormal
      )
    ]
    <> modeLeavingKeys (floatingWindowsKeys m)
    <> windowFocusKeys m
    <> moveKeys

-- | A tag name for the floating window that appears on all workspaces
stickyWindowTag ∷ String
stickyWindowTag = "sticky-window"

doKeysModeLabel ∷ String
doKeysMode ∷ Modal.Mode
(doKeysModeLabel, doKeysMode) = (label, mode) where
  label = "Do"
  mode = Modal.mode label $ \XConfig { XMonad.modMask = m } →
    let
      leaveMode = (>> Modal.exitMode)

      cursorToDisplayKeys = Map.fromList
        [ ((mask, key), wrap $ XMonad.spawn [qms| cursor-to-display -d {n} |])
        | (n, key) ← zip [1 ∷ Word ..] [XMonad.xK_z, XMonad.xK_x, XMonad.xK_c, XMonad.xK_v]
        , (mask, wrap) ← [(m, (>> Modal.exitMode)), (0, id)]
        ]

      closeWindowKeys = Map.fromList
        -- Mnemonic: ‘q’ is for ‘Quit’.
        [ ((mask, XMonad.xK_q), wrap operation)
        | (mask, operation, wrap) ←
            -- Close focused window keys
            [ (m, XMonad.kill, leaveMode)
            , (0, XMonad.kill, id)
            -- Forcefully close focused window (send SIGKILL to the window owner)
            , (m .|. a, XMonad.spawn cmdExterminate, leaveMode)
            , (s, XMonad.spawn cmdExterminate, id)
            ]
        ]

      -- | Sending SIGSTOP and SIGCONT to window owners keys
      holdResumeKeys = Map.fromList
        [ ((mask, key), wrap $ XMonad.spawn cmd)
        | (key, cmdNormal, cmdRecursive) ←
            -- Mnemonic: ‘h’ is for ‘Hold’.
            [ (XMonad.xK_h, cmdPause, cmdPauseRecursive)
            -- Mnemonic: ‘j’ is for ‘Join’.
            , (XMonad.xK_j, cmdResume, cmdResumeRecursive)
            ]
        , (mask, cmd, wrap) ←
            [ (m, cmdNormal, leaveMode)
            , (0, cmdNormal, id)
            , (m .|. a, cmdRecursive, leaveMode)
            , (s, cmdRecursive, id)
            ]
        ]
    in
    Map.fromList
    [ ((0, XMonad.xK_Escape), Modal.exitMode)

    -- Restart XMonad applying the new configuration.
    -- Mnemonic: ‘r’ is for ‘Restart’.
    , ((m, XMonad.xK_r), XMonad.restart "xmonad" True >> Modal.exitMode)
    -- Exit XMonad. Log out from the X session.
    -- Mnemonic: ‘e’ is for ‘Exit’.
    -- TODO: Come up with a solution to trigger “io exitSuccess” instead when YES is pressed.
    , ( (m, XMonad.xK_e)
      , XMonad.spawn [qns|
          i3-nagbar
            -t warning
            -m 'Do you really want to terminate XMonad? This will end your X session!'
            -B 'Yes, terminate XMonad' 'kill -TERM -- $(pidof xmonad)'
        |] >> Modal.exitMode
      )

    -- Mnemonic: ‘a’ is for ‘Autostart’
    , ((m, XMonad.xK_a), XMonad.spawn "autostart-setup" >> Modal.exitMode)
    -- Mnemonic: ‘i’ is for ‘Input’
    , ((m, XMonad.xK_i), XMonad.spawn "input-setup" >> Modal.exitMode)

    -- Mnemonic: ‘g’ is for ‘Go’
    , ((m, XMonad.xK_g), XMonad.spawn "place-cursor-at" >> Modal.exitMode)
    , ((0, XMonad.xK_g), XMonad.spawn "place-cursor-at")

    -- Make the window appear on other workspaces.
    -- Mnemonic: ‘f’ is for ‘Force’ (force a window to appear on all workspaces).
    -- FIXME: It works weird or rather doesn’t work/misbehaves with multiple screens.
    --        I’ve been told this bug is fixed in XMonad “master” branch.
    --        Have to try later, after next XMonad release.
    , ((m, XMonad.xK_f), toggleStickyWindow >> Modal.exitMode)
    , ((0, XMonad.xK_f), toggleStickyWindow)

    -- Mnemonic: ‘b’ is for ‘Border’
    , ( (m, XMonad.xK_b)
      , Spacing.toggleWindowSpacingEnabled >> Spacing.toggleScreenSpacingEnabled >> Modal.exitMode
      )
    , ( (0, XMonad.xK_b)
      , Spacing.toggleWindowSpacingEnabled >> Spacing.toggleScreenSpacingEnabled
      )
    , ((a, XMonad.xK_b), Spacing.decWindowSpacing 1 >> Spacing.decScreenSpacing 1)
    , ((s, XMonad.xK_b), Spacing.incWindowSpacing 1 >> Spacing.incScreenSpacing 1)

    -- Mnemonic: ‘s’ is for ‘Shutter’
    , ((m, XMonad.xK_s), XMonad.spawn "shutter" >> Modal.exitMode)
    , ((0, XMonad.xK_s), XMonad.spawn "shutter")

    -- # mnemonic: ‘u’ is a letter of ‘tmUx’
    -- bindsym $m+u [class="^Termite" title="tmux"] focus; mode "default"
    -- bindsym    u [class="^Termite" title="tmux"] focus
    -- # mnemonic: ‘y’ visually looks like ‘v’
    -- bindsym $m+y [class="^Termite" title="vim"]  focus; mode "default"
    -- bindsym    y [class="^Termite" title="vim"]  focus

    -- # jump to audacious windows (when it's float, winamp classic interface)
    -- # mnemonic: ‘m’ is for ‘Music’
    -- bindsym $m+m [class="^Audacious$" title="Audacious$" floating] focus; mode "default"
    -- bindsym    m [class="^Audacious$" title="Audacious$" floating] focus
    -- # mnemonic: ‘p’ is for ‘Playlist’
    -- bindsym $m+p [class="^Audacious$" title="^Audacious Playlist Editor$" floating] focus; mode "default"
    -- bindsym    p [class="^Audacious$" title="^Audacious Playlist Editor$" floating] focus

    -- Mnemonic: ‘n’ is for ‘Negative’
    , ((m, XMonad.xK_n), XMonad.spawn "timeout 2s invert-window-colors app all" >> Modal.exitMode)
    , ((0, XMonad.xK_n), XMonad.spawn "timeout 2s invert-window-colors app all")

    -- Rename current workspace to temporary name.
    -- Mnemonic: ‘t’ is for ‘Temporary’.
    -- bindsym $m+t exec $tmp_workspace; mode "default"
    -- bindsym    t exec $tmp_workspace
    ]
    <> modeLeavingKeys (floatingWindowsKeys m)
    <> closeWindowKeys
    <> holdResumeKeys
    <> cursorToDisplayKeys

workspaceKeysModeLabel ∷ String
workspaceKeysMode ∷ Modal.Mode
(workspaceKeysModeLabel, workspaceKeysMode) = (label, mode) where
  label = "Workspace"
  mode = Modal.mode label $ \XConfig { XMonad.modMask = m, XMonad.workspaces = ws } →
    Map.fromList
    [ ((0, XMonad.xK_Escape), Modal.exitMode)
    -- bindsym    i                   workspace im; mode "default"
    -- bindsym $s+i move container to workspace im; mode "default"

    -- bindsym    m                   workspace music; mode "default"
    -- bindsym $s+m move container to workspace music; mode "default"

    -- bindsym    v                   workspace video; mode "default"
    -- bindsym $s+v move container to workspace video; mode "default"

    -- bindsym    g                   workspace main; mode "default"
    -- bindsym $s+g move container to workspace main; mode "default"

    -- bindsym    c                   workspace code; mode "default"
    -- bindsym $s+c move container to workspace code; mode "default"

    , ((0, XMonad.xK_z), CycleWS.moveTo CycleWS.Prev CycleWS.hiddenWS)
    , ((0, XMonad.xK_x), CycleWS.moveTo CycleWS.Next CycleWS.hiddenWS)
    , ((s, XMonad.xK_z), CycleWS.shiftTo CycleWS.Prev CycleWS.hiddenWS)
    , ((s, XMonad.xK_x), CycleWS.shiftTo CycleWS.Next CycleWS.hiddenWS)

    , ((m, XMonad.xK_b), CycleWS.toggleWS >> Modal.exitMode)
    , ((0, XMonad.xK_b), CycleWS.toggleWS)

    -- bindsym n exec $new_workspace; mode "default"
    ]
    <> modeLeavingKeys (floatingWindowsKeys m)
    <> navigationBetweenDisplaysKeys (m, m .|. a, a)
    <> modeLeavingKeys (navigationBetweenWorkspacesKeys ws (0, s))

mouseCursorKeysModeLabel ∷ String
mouseCursorKeysMode ∷ Modal.Mode
(mouseCursorKeysModeLabel, mouseCursorKeysMode) = (label, mode) where
  label = "Mouse cursor"
  mode = Modal.mode label $ \XConfig { XMonad.modMask = m } →
    let
      placeCursorAtKeys = Map.fromList
        [ ((0, key), XMonad.spawn [qms| place-cursor-at {position} |])
        | (key, position) ←
            [ (XMonad.xK_q, "LT")
            , (XMonad.xK_a, "LC")
            , (XMonad.xK_z, "LB")
            , (XMonad.xK_w, "CT")
            , (XMonad.xK_s, "CC")
            , (XMonad.xK_x, "CB")
            , (XMonad.xK_e, "RT")
            , (XMonad.xK_d, "RC")
            , (XMonad.xK_c, "RB")
            ]
        ]

      cursorMoveKeys = Map.fromList
        [ ((mask, key), XMonad.spawn [qms| xdotool mousemove_relative -- {step x n} {step y n} |])
        | (mask, n) ← [(0, 10), (a, 50), (s, 100)]
        , (key, (x, y)) ← zip hjklKeys [(LT, EQ), (EQ, GT), (EQ, LT), (GT, EQ)]
        ]
        where
          step axis n = mconcat
            [ case axis of EQ → "" ; LT → "-" ; GT → "+"
            , show @Word (if axis == EQ then 0 else n)
            ]

      mouseButtonsKeys = Map.fromList
        [ ((mask, key), wrap $ XMonad.spawn [qms| xdotool mousedown {n} mouseup {n} |])
        | (mask, wrap) ← [(0, id), (m, (>> Modal.exitMode))]
        , (key, n ∷ Word) ← [(XMonad.xK_r, 2), (XMonad.xK_f, 1), (XMonad.xK_v, 3)]
        ]
    in
    Map.singleton (0, XMonad.xK_Escape) Modal.exitMode
    <> modeLeavingKeys (floatingWindowsKeys m)
    <> windowFocusKeys m
    <> navigationBetweenDisplaysKeys (0, a, s)
    <> placeCursorAtKeys
    <> cursorMoveKeys
    <> mouseButtonsKeys

-- | Add all modes to the XMonad config
withKeysModes ∷ XConfig layout → XConfig layout
withKeysModes = HModal.modal . mconcat $
  [ [doKeysMode, workspaceKeysMode, mouseCursorKeysMode]
  , [resizeKeysMode x | x ← [minBound .. maxBound]]
  , [positioningKeysMode x | x ← [minBound .. maxBound]]
  ]

-- * Manage hook

myManageHook ∷ XMonad.ManageHook
myManageHook = XMonad.composeAll
  [ XMonad.className =? "Gpaste-gui" --> doCenterFloat
  , XMonad.className =? "gnome-calculator" --> doCenterFloat

  , XMonad.className =? "calfjackhost" --> doTile

  -- Moving to last workspace
  , XMonad.className =? "thunderbird" --> moveTo imWsLabel
  , XMonad.className =? "nheko" --> moveTo imWsLabel
  , XMonad.className =? "Psi+" --> moveTo imWsLabel

  , isAbove --> XMonad.doFloat
  , isStickyWindow --> XMonad.doF copyToAll
  ]
  where
    moveTo = XMonad.doF . W.shift
    doTile = XMonad.ask >>= XMonad.doF . W.sink

    imWsLabel = "10"

    -- | Get the @_NET_WM_STATE@ property as a list of atoms
    --
    -- See https://unix.stackexchange.com/a/708140
    getNetWMState ∷ XMonad.Window → X [XMonad.Atom]
    getNetWMState window = do
      atom <- XMonad.getAtom "_NET_WM_STATE"
      maybe [] (fmap fromIntegral) <$> getProp32 atom window

    hasNetWMState ∷ String → XMonad.Window → X Bool
    hasNetWMState state window = elem <$> XMonad.getAtom state <*> getNetWMState window

    isAbove ∷ XMonad.Query Bool
    isAbove = XMonad.liftX . hasNetWMState "_NET_WM_STATE_ABOVE" =<< XMonad.ask

    isStickyWindow ∷ XMonad.Query Bool
    isStickyWindow = XMonad.liftX . hasNetWMState "_NET_WM_STATE_STICKY" =<< XMonad.ask

-- * Helpers

data RofiMode = RofiModeRun | RofiModeDRun | RofiModeWindow
rofiModeForCmd ∷ RofiMode → String
rofiModeForCmd = \case RofiModeRun → "run" ; RofiModeDRun → "drun" ; RofiModeWindow → "window"

data RofiTheme = RofiThemeDark | RofiThemeLight
rofiThemeForCmd ∷ RofiTheme → String
rofiThemeForCmd = \case RofiThemeDark → "gruvbox-dark" ; RofiThemeLight → "gruvbox-light-soft"

rofiCmd ∷ RofiMode → RofiTheme → String
rofiCmd mode theme = unwords [ "rofi -show", rofiModeForCmd mode, "-theme", rofiThemeForCmd theme ]

-- ** Keys

-- | Make all key binding in a given set to trigger mode exit after they are pressed
modeLeavingKeys ∷ Keys → Keys
modeLeavingKeys = Map.map (>> Modal.exitMode)

-- ** Operations on windows

-- | Toggle window floating state
toggleFloatWindow ∷ XMonad.Window → X ()
toggleFloatWindow w = go where
  go = XMonad.windows . toggle . snd =<< XMonad.floatLocation w

  toggle ∷ W.RationalRect → W.StackSet i l XMonad.Window s sd → W.StackSet i l XMonad.Window s sd
  toggle r ss = ss { W.floating = Map.alter f w (W.floating ss) }
    where
      f (Just _) = Nothing -- Remove from floating windows list if it’s a floating window
      f Nothing = Just r -- Add to floating windows list if it’s a tiled window

-- | Toggle window stickyness state
--
-- Window being “sticky” means it appears on all workspaces.
-- Useful for floating windows that can sit in a corner of a screen.
toggleStickyWindow ∷ X ()
toggleStickyWindow = XMonad.withFocused $ \window → do
  XMonad.ifM
    (TagWindows.hasTag stickyWindowTag window)
    (killAllOtherCopies >> TagWindows.delTag stickyWindowTag window)
    (XMonad.windows copyToAll >> TagWindows.addTag stickyWindowTag window)

-- | Check if the window is a floating one
isWindowFloating ∷ XMonad.Window → X Bool
isWindowFloating window = XMonad.gets (Map.member window . W.floating . XMonad.windowset)

-- | Toggle the focus between tiled and floating windows
toggleFloatingTiledFocus ∷ X ()
toggleFloatingTiledFocus =
  XMonad.withFocused $ \window → do
    isFloating ← isWindowFloating window
    windowSet ← XMonad.gets XMonad.windowset
    -- “Current” means all windows of current screen/workspace
    let allCurrentWindows = W.integrate' . W.stack . W.workspace . W.current $ windowSet
    let allFloatingWindows = Map.keys . W.floating $ windowSet
    let (currentFloating, currentTiled) = partition (`elem` allFloatingWindows) allCurrentWindows
    let current = if isFloating then currentTiled else currentFloating
    maybe (pure ()) XMonad.focus (listToMaybe current)

-- | Focus last window in the stack
--
-- Kind of the opposite of "W.focusMaster".
focusLast ∷ W.StackSet i l XMonad.Window s sd -> W.StackSet i l XMonad.Window s sd
focusLast = W.modify' $ \c → case c of
  W.Stack t ls (NE.nonEmpty → fmap NE.reverse → Just (lastWindow NE.:| rs)) →
    W.Stack lastWindow (rs <> (t : ls)) []
  _ → c
