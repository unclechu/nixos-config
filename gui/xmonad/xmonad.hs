-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

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
import XMonad.Hooks.ManageDocks (avoidStruts)
-- import XMonad.Layout.SimplestFloat (simplestFloat)
import XMonad.Hooks.ManageHelpers (doCenterFloat)

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

  = EwmhDesktops.ewmh
  . withKeysModes
  . myConfig

myConfig ∷ XConfig _layoutA → XConfig _layoutB
myConfig x = x
  { modMask = XMonad.mod4Mask -- Super key
  , workspaces = show <$> [1 .. 10 ∷ Word]

  , terminal = "alacritty"
  , normalBorderColor = "#222222"
  , focusedBorderColor = "#ff0000"

  -- This startup script is provided by my NixOS configuration
  , startupHook = XMonad.spawn "autostart-setup"

  , layoutHook = myLayout
  , manageHook = myManageHook
  , keys = defaultModeKeys
  , focusFollowsMouse = False
  , clickJustFocuses = False
  }

myLayout ∷ _layout
myLayout = go where
  go =
    avoidStruts (tiled ||| XMonad.Mirror tiled ||| tabbed ||| XMonad.Full)
    -- ||| simplestFloat
    ||| noBorders XMonad.Full -- Fullscreen-like

  tiled = ResizableTile.ResizableTall 1 (3 / 100) (1 / 2) []
  tabbed = Tabbed.tabbed Tabbed.shrinkText myTabTheme

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

-- ** Key binding modes

-- | Key mapping set
type Keys = Map.Map (XMonad.KeyMask, XMonad.KeySym) (X ())

a, s ∷ XMonad.KeyMask
a = XMonad.mod1Mask -- “a” for “Alt”
s = XMonad.shiftMask

hjklKeys, arrowKeys, displayKeys ∷ [XMonad.KeySym]
hjklKeys = [XMonad.xK_h, XMonad.xK_j, XMonad.xK_k, XMonad.xK_l]
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
  operations = [W.focusMaster, W.focusDown, W.focusUp]
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

  -- TODO: Try to port this from my i3wm config.
  --       In XMonad you can just focus a floating window by moving between all windows.
  -- # change focus between tiling / floating windows
  -- bindsym $m+space focus mode_toggle
  ]

type WorkspaceLabel = String

-- | Walking between workspaces and moving windows between workspaces
navigationBetweenWorkspacesKeys ∷ [WorkspaceLabel] → (XMonad.KeyMask, XMonad.KeyMask) → Keys
navigationBetweenWorkspacesKeys ws (switchMask, moveMask) = Map.fromList
  [ ((mask, key), XMonad.windows $ operation workspace)
  | (workspace, key) <- zip ws $ [XMonad.xK_1 .. XMonad.xK_9] <> [XMonad.xK_0]
  , (mask, operation) <-
      [ (switchMask, W.greedyView) -- %! Switch to workspace
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

  Map.fromList
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

  -- Making screenshots
  , ((0, XMonad.xK_Print), XMonad.spawn "gnome-screenshot")
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

  -- TODO: Port from my i3wm config.
  -- TODO: Try out the i3wm-like layout.
  -- bindsym $m+c split v
  -- bindsym $m+v split h

  -- TODO: Come up with an XMonad alternative of this i3wm binding.
  --       Maybe just jump to the fullscreen-ish layout?
  -- bindsym $m+f fullscreen toggle

  -- TODO: Try to port this from my i3wm config.
  --       I’ve seen in the past there are some extensions that simulate those i3-like layout splits.
  --   bindsym $m+s layout stacking
  --   bindsym $m+w layout tabbed
  --   bindsym $m+e layout toggle split
  -- Until I figure out how to do i3-like layouting
  -- I use these keys for regular XMonad layout rotation.
  -- , ((m, XMonad.xK_s), _)
  , ((m, XMonad.xK_w), XMonad.setLayout layout) -- Reset layout state
  , ((m, XMonad.xK_e), XMonad.sendMessage XMonad.NextLayout)

  -- TODO: Try to port this from my i3wm config.
  --       It only makes sense.
  -- # focus the parent container
  -- bindsym $m+a focus parent
  -- # focus the child container
  -- bindsym $m+q focus child

  -- TODO: Find out if I can do dynamic workspaces in XMonad like in i3wm.
  -- bindsym $m+b exec $new_workspace

  -- XMonad master windows amount manipulation
  , ((m, XMonad.xK_c), XMonad.sendMessage $ XMonad.IncMasterN 1)
  , ((m .|. a, XMonad.xK_c), XMonad.sendMessage $ XMonad.IncMasterN (-1))
  , ((m .|. s, XMonad.xK_c), XMonad.refresh)

  -- Switching to different modes
  , ((m, XMonad.xK_d), Modal.setMode doKeysModeLabel)
  , ((m, XMonad.xK_g), Modal.setMode workspaceKeysModeLabel)
  , ((m, XMonad.xK_r), Modal.setMode $ resizeKeysModeToLabel ResizeKeysModeNormal)
  , ((m, XMonad.xK_t), Modal.setMode $ positioningKeysModeToLabel PositioningKeysModeNormal)
  , ((m .|. a, XMonad.xK_m), Modal.setMode mouseCursorKeysModeLabel)
  ]
  <> windowFocusKeys m
  <> windowMoveKeys (m .|. a)
  <> floatingWindowsKeys m
  <> navigationBetweenWorkspacesKeys ws (m, m .|. a)
  <> rotateWorkspacesKeys
  <> navigationBetweenDisplaysKeys (m, m .|. a, m .|. s)
  <> movingWorkspacesKeys
  where
    -- | Next/previous workspace switching keys
    --
    -- TODO: Port from my i3wm config:
    -- @
    -- bindsym $m+z workspace prev_on_output
    -- bindsym $m+x workspace next_on_output
    -- bindsym $m+$a+z move container to workspace prev_on_output
    -- bindsym $m+$a+x move container to workspace next_on_output
    -- @
    rotateWorkspacesKeys = Map.empty

    -- | Moving workspaces to specific displays
    --
    -- TODO: Port from my i3wm config:
    -- @
    -- bindsym $m+comma     move workspace to output left
    -- bindsym $m+$s+comma  move workspace to output down
    -- bindsym $m+period    move workspace to output right
    -- bindsym $m+$s+period move workspace to output up
    -- @
    movingWorkspacesKeys = Map.empty

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
        [ f XMonad.Shrink
        , f ResizableTile.MirrorShrink
        , f ResizableTile.MirrorExpand
        , f XMonad.Expand
        ]
        where
          f = XMonad.sendMessage

      growShrink mask repeats = Map.fromList
        [ ((mask, key), replicateM_ repeats operation)
        | directionKeys ← [hjklKeys, arrowKeys]
        , (key, operation) ← zip directionKeys operations
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
    ]
    <> modeLeavingKeys (floatingWindowsKeys m)
    <> windowFocusKeys m
    <> growShrink 0 repeatN
    <> growShrink a (2 * repeatN)
    <> growShrink s (3 * repeatN)

data PositioningKeysMode
  = PositioningKeysModeNormal
  | PositioningKeysModeBigSteps
  deriving (Eq, Show, Enum, Bounded)

positioningKeysModeToLabel ∷ PositioningKeysMode → String
positioningKeysModeToLabel PositioningKeysModeNormal = "Positioning"
positioningKeysModeToLabel PositioningKeysModeBigSteps = "Positioning (big steps)"

positioningKeysModeToStepRepeats ∷ Num a ⇒ PositioningKeysMode → a
positioningKeysModeToStepRepeats PositioningKeysModeNormal = 1
positioningKeysModeToStepRepeats PositioningKeysModeBigSteps = 5

positioningKeysMode ∷ PositioningKeysMode → Modal.Mode
positioningKeysMode mode = go where
  label = positioningKeysModeToLabel mode
  go = Modal.mode label $ \XConfig { XMonad.modMask = m } →
    let
      _repeatN = positioningKeysModeToStepRepeats mode
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

    -- bindsym h move left
    -- bindsym j move down
    -- bindsym k move up
    -- bindsym l move right

    -- bindsym $a+h move left  ; move left
    -- bindsym $a+j move down  ; move down
    -- bindsym $a+k move up    ; move up
    -- bindsym $a+l move right ; move right

    -- bindsym $s+h move left  ; move left  ; move left  ; move left
    -- bindsym $s+j move down  ; move down  ; move down  ; move down
    -- bindsym $s+k move up    ; move up    ; move up    ; move up
    -- bindsym $s+l move right ; move right ; move right ; move right

    -- bindsym Left  move left
    -- bindsym Down  move down
    -- bindsym Up    move up
    -- bindsym Right move right
    ]
    <> modeLeavingKeys (floatingWindowsKeys m)
    <> windowFocusKeys m

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

    -- # mnemonic: ‘f’ is for ‘Force’ (force a window to appear on all workspaces)
    -- bindsym $m+f sticky toggle; mode "default"
    -- bindsym    f sticky toggle

    -- # mnemonic: ‘b’ is for ‘Border’
    -- bindsym $m+b border toggle; mode "default"
    -- bindsym    b border toggle

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

    -- bindsym    z                   workspace prev_on_output
    -- bindsym $s+z move container to workspace prev_on_output
    -- bindsym    x                   workspace next_on_output
    -- bindsym $s+x move container to workspace next_on_output

    -- bindsym b workspace back_and_forth; mode "default"
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

  -- Moving to last workspace
  , XMonad.className =? "thunderbird" --> moveTo imWsLabel
  , XMonad.className =? "nheko" --> moveTo imWsLabel
  , XMonad.className =? "Psi+" --> moveTo imWsLabel
  ]
  where
    moveTo = XMonad.doF . W.shift
    imWsLabel = "10"

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