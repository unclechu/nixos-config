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

{-# OPTIONS_GHC -Wall -Wno-partial-type-signatures -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use <&>" #-}

{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedRecordDot #-}

import XMonad (XConfig (..), X, (.|.), (|||), (-->), (=?), (<&&>))
import qualified XMonad
import qualified XMonad.Hooks.Modal as HModal
import qualified XMonad.Hooks.EwmhDesktops as EwmhDesktops
import qualified Data.Map.Strict as Map
import Text.InterpolatedString.QM (qms, qns, qmb)
import qualified Graphics.X11.ExtraTypes.XF86 as XF86
import qualified XMonad.StackSet as W
import qualified XMonad.Hooks.Modal as Modal
import Control.Monad (replicateM_, when, guard, void)
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
import qualified XMonad.Hooks.InsertPosition as InsertPosition
import Data.List (partition, intercalate)
import Data.Maybe (listToMaybe, fromMaybe)
import XMonad.Hooks.TaffybarPagerHints (pagerHints)
import qualified Data.List.NonEmpty as NE
import System.Environment (getEnvironment, lookupEnv, unsetEnv, getArgs)
import Text.Read (readMaybe)
import qualified System.Process.Typed as ProcT
import Network.HostName (getHostName)
import Data.Functor ((<&>))
import XMonad.Layout.PerWorkspace (onWorkspace)
import System.Posix (getEnv)
import System.Posix.Process (executeFile)
import System.FilePath ((</>))
import qualified XMonad.Hooks.DynamicLog as DynamicLog
import XMonad.Util.Loggers (Logger)
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception as E
import qualified XMonad.Hooks.UrgencyHook as UrgencyHook
import qualified Data.IORef as IORef
import qualified System.IO as SysIO
import Control.Monad.Fix (fix)
import qualified System.IO.Error as IOError
import qualified System.Directory as Dir
import qualified System.Posix.Process as PosixProc
import qualified System.Posix.Files as PosixFiles
import qualified Data.Time.Clock.POSIX as POSIX
import qualified Graphics.X11 as X11
import qualified Graphics.X11.Xlib.Extras as X11Extras
import GHC.Internal.System.Environment (getProgName)
import qualified XMonad.Hooks.ServerMode as ServerMode
import qualified System.Timeout as Timeout
import Data.Bool (bool)
import qualified System.Posix.Signals as PosixSignals

main ∷ IO ()
main = do
  getArgs >>= \case
    subCommand : xs
      | subCommand == ctlSubcommand &&
        ( xs == []
        || (case xs of [x] → x `elem` ["help", "--help", "-h", "-?"]; _ → False)
        ) → do
          progName ← getProgName
          SysIO.hPutStr SysIO.stderr (ctlHelp progName)
      | otherwise → ctl xs
    _ → startXMonad

  where
    ctl commandXs =
      case parseXMonadAction (unwords commandXs) of
        Nothing → fail $ "Unrecognized command: " <> show commandXs
        Just x → sendXMonadActionToX11 x

    ctlSubcommand = "ctl" ∷ String

    ctlHelp ∷ String → String
    ctlHelp progName
      = unlines
      . (["Command examples:"] <>)
      . fmap (("  " <> progName <> " " <> ctlSubcommand <> " ") <>)
      . fmap xMonadActionToAtomString
      $ [ ToggleStickyWindow
        , ToggleFloatingWindow
        , CycleLayout
        , ResetLayout
        , ResetMode
        , (SwitchWorkspace . fromMaybe mempty . listToMaybe) myWorkspaces
        ]

    startXMonad ∷ IO ()
    startXMonad = do
      writeFile "/tmp/xmonad.log" "XMonad initializes\n"
      xdgRuntimeDir ← mkXdgRuntimeDir
      let polybarStateFiles = mkPolybarStateFiles xdgRuntimeDir

      polybarRunScript ← do
        let envVarName = "POLYBAR_RUN_SCRIPT"
        !x ← fromMaybe "run-polybar" <$> getEnv envVarName
        x <$ unsetEnv envVarName

      withPolybar polybarStateFiles polybarRunScript $ \polybarInterface → do
        opts ← mkOptions xdgRuntimeDir

        let
          myStartupHook = do
            autoStartScript opts
            XMonad.io polybarInterface.polybar_release

        writeFile "/tmp/xmonad.log" "XMonad starts\n"
        XMonad.xmonad $ completeConfig opts polybarInterface myStartupHook

data Options = Options
  { options_startMode ∷ XMonadStartMode
  , options_saveMoreSpace ∷ Bool
  , options_tallMasters ∷ Int
  , options_executableType ∷ XMonadExecutableType
  , options_xdgRuntimeDir ∷ XdgRuntimeDir
  }

mkOptions ∷ XdgRuntimeDir → IO Options
mkOptions xdgRuntimeDir =
  Options
    <$> getRestartMode
    <*> isSavingMoreSpaceNeeded
    <*> pure 1
    <*> isDevXMonad
    <*> pure xdgRuntimeDir
  where
    isDevXMonad = do
      let envVarName = "XMONAD_DEV"
      !isDev ← getEnv envVarName <&> \case Just "1" → True; _ → False
      unsetEnv envVarName
      pure $ if isDev then Dev else Normal

getRestartMode ∷ IO XMonadStartMode
getRestartMode = do
  -- Try to look if there’s restart mode value passed by an environment variable
  raw ← lookupEnv startModeMarkerEnvName

  -- We don’t need it to keep forwarding for the next restart.
  -- Just removing the value in case it was set.
  maybe (pure ()) (const $ unsetEnv startModeMarkerEnvName) raw

  -- Parse the restart mode (or just use default mode value if parsing fails)
  pure $ fromMaybe XMonad.def $ readMaybe @XMonadStartMode =<< raw

-- | Check if saving more usable screen space is needed (e.g. turn off spacing).
isSavingMoreSpaceNeeded ∷ IO Bool
isSavingMoreSpaceNeeded =
  getHostName <&> \case
    -- This machine is very old, the screen DPI is low,
    -- need as much usable space as I can get.
    "wenzel-rusty-chunk" → True
    _ → False

xmonadActionXMessageType ∷ String
xmonadActionXMessageType = "XMONAD_ACTION"

data XMonadAction
  = ToggleStickyWindow
  | ToggleFloatingWindow
  | CycleLayout
  | ResetLayout
  | ResetMode
  | SwitchWorkspace String -- The @String@ should not contain spaces
  deriving (Eq, Show)

xMonadActionToAtomString ∷ XMonadAction → String
xMonadActionToAtomString = \case
  ToggleStickyWindow → "toggle-sticky-window"
  ToggleFloatingWindow → "toggle-floating-window"
  CycleLayout → "cycle-layout"
  ResetLayout → "reset-layout"
  ResetMode → "reset-mode"
  -- Note that @ws@ here must be an element of @myWorkspaces@
  SwitchWorkspace ws → unwords ["switch-workspace", ws]

parseXMonadAction ∷ String → Maybe XMonadAction
parseXMonadAction x
  | x == f ToggleStickyWindow = Just ToggleStickyWindow
  | x == f ToggleFloatingWindow = Just ToggleFloatingWindow
  | x == f CycleLayout = Just CycleLayout
  | x == f ResetLayout = Just ResetLayout
  | x == f ResetMode = Just ResetMode
  | otherwise = case words x of
      ["switch-workspace", ws] | ws `elem` myWorkspaces → Just (SwitchWorkspace ws)
      _ → Nothing
  where f = xMonadActionToAtomString

-- | Send an Atom message to X root window (that XMonad can catch and handle)
--
-- For the `ctl` calls.
sendXMonadActionToX11 ∷ XMonadAction → IO ()
sendXMonadActionToX11 (xMonadActionToAtomString → action) = do
  display ← X11.openDisplay ""
  rootWnd ← X11.rootWindow display (X11.defaultScreen display)

  -- These Atoms live forever within one X11 session.
  -- But if the specified string already exists in the Atom table
  -- then the atom is reused, so the old Atom ID is found and returned.
  -- Just make sure that the commands that are sent are predictable and limited
  -- to a somewhat known set.
  messageTypeAtom ← X11.internAtom display xmonadActionXMessageType False
  commandAtom ← X11.internAtom display action False

  X11.allocaXEvent $ \eventPtr → do
    X11Extras.setEventType eventPtr X11.clientMessage
    X11Extras.setClientMessageEvent
      eventPtr rootWnd messageTypeAtom 32 commandAtom 0
    X11.sendEvent display rootWnd False X11.structureNotifyMask eventPtr
    X11.sync display False

  X11.closeDisplay display

-- | Startup script that is provided by my NixOS configuration.
--
-- Blocking! Waits for the script to finish.
autoStartScript ∷ Options → XMonad.X ()
autoStartScript opts = do
  case options_startMode opts of
    Shallow → pure ()
    Full → do
      XMonad.io $ report "Creating FIFO for autostart-setup script"
      withFifoResponse opts.options_xdgRuntimeDir "autostart-wait" $ \fifoPath getFifoLine → do
        XMonad.io $ report "Starting autostart-setup script"
        XMonad.spawn [qmb|
          autostart-setup 1>&2
          status=$?
          printf '%s\n' "$status" > {shellQuote fifoPath}
          exit "$status"
        |]

        XMonad.io $ report "Waiting for autostart-setup script FIFO report"
        getFifoLine (Just $ _seconds 5) $ \case
          Left (e ∷ E.SomeException) →
            report $
              "autostart-setup script FIFO read failed: "
              <> E.displayException e

          Right (readMaybe → Just (exitCode ∷ Int)) →
            case exitCode of
              0 → report "autostart-setup script finished successfully"
              n →
                report $
                  "autostart-setup script failed with exit code: " <> show n

          Right x →
            report $
              "autostart-setup script failed with unexpected FIFO report: "
              <> show x

  where
    report = writeFile "/tmp/xmonad-autostart-setup.log" . (<> "\n")

newtype XdgRuntimeDir = XdgRuntimeDir { unXdgRuntimeDir ∷ String }
  deriving (Eq, Show)

mkXdgRuntimeDir ∷ IO XdgRuntimeDir
mkXdgRuntimeDir =
  getEnv envVarName >>=
    maybe (fail $ "Failed to get " <> envVarName) pure
      . (>>= \x → XdgRuntimeDir x <$ guard (x /= mempty))
  where envVarName = "XDG_RUNTIME_DIR"

-- | Polybar state files information
--
-- Pairs of Polybar module name + state file path
data PolybarStateFiles = PolybarStateFiles
  { polybar_workspacesFile ∷ (String, FilePath)
  , polybar_layoutFile ∷ (String, FilePath)
  , polybar_modeFile ∷ (String, FilePath)
  , polybar_windowFlagsFile ∷ (String, FilePath)
  }

mkPolybarStateFiles ∷ XdgRuntimeDir → PolybarStateFiles
mkPolybarStateFiles (unXdgRuntimeDir → dir) = PolybarStateFiles
  { polybar_workspacesFile = ("xmonad-workspaces", dir </> "xmonad-polybar-workspaces")
  , polybar_layoutFile = ("xmonad-layout", dir </> "xmonad-polybar-layout")
  , polybar_modeFile = ("xmonad-mode", dir </> "xmonad-polybar-mode")
  , polybar_windowFlagsFile = ("xmonad-window-flags", dir </> "xmonad-polybar-window-flags")
  }

data PolybarInterface = PolybarInterface
  { polybar_release ∷ IO ()
  , polybar_reportWorkspaces ∷ String → IO ()
  , polybar_reportLayout ∷ String → IO ()
  , polybar_reportMode ∷ String → IO ()
  , polybar_reportWindowFlags ∷ String → IO ()
  }

withPolybar ∷ PolybarStateFiles → FilePath → (PolybarInterface → IO ()) → IO ()
withPolybar stateFiles polybarRunScript runXMonad = do
  startPolybarLatch ← MVar.newEmptyMVar @()

  -- Writing handle + writing lock wrapper
  polybarIpcHandle ← IORef.newIORef (Nothing @(SysIO.Handle, IO () → IO ()))

  Async.withAsync (polybarThread startPolybarLatch polybarIpcHandle) $ \polybarAsync →
    withPolybarReporter polybarIpcHandle stateFiles.polybar_workspacesFile $ \reportWorkspaces →
      withPolybarReporter polybarIpcHandle stateFiles.polybar_layoutFile $ \reportLayout →
        withPolybarReporter polybarIpcHandle stateFiles.polybar_modeFile $ \reportMode →
          withPolybarReporter polybarIpcHandle stateFiles.polybar_windowFlagsFile $ \reportWindowFlags →
            let
              polybarInterface = PolybarInterface
                { polybar_release = startPolybar startPolybarLatch
                , polybar_reportWorkspaces = reportWorkspaces
                , polybar_reportLayout = reportLayout
                , polybar_reportMode = reportMode
                , polybar_reportWindowFlags = reportWindowFlags
                }
            in
              runXMonad polybarInterface `E.finally` Async.cancel polybarAsync

  where
    report = writeFile "/tmp/xmonad-polybar.log" . (<> "\n")

    runPolybarProcess
      = ProcT.setStdout ProcT.nullStream
      . ProcT.setStderr ProcT.inherit
      . ProcT.setStdin ProcT.createPipe
      $ ProcT.proc polybarRunScript []

    startPolybar ∷ MVar.MVar () → IO ()
    startPolybar = void . flip MVar.tryPutMVar ()

    polybarThread
      ∷ MVar.MVar ()
      → IORef.IORef (Maybe (SysIO.Handle, IO () → IO ()))
      → IO ()
    polybarThread startPolybarLatch polybarIpcHandle = do
      report "Waiting for startupHook trigger before starting Polybar"

      -- Wait for startupHook to call the startPolybar
      MVar.takeMVar startPolybarLatch

      report $ "Starting Polybar script " <> show polybarRunScript

      let
        start =
          ProcT.withProcessTerm runPolybarProcess $ \procHandle → do
            lock ← MVar.newMVar ()
            IORef.atomicWriteIORef polybarIpcHandle . Just $
              (ProcT.getStdin procHandle, MVar.withMVar @() lock . const)

            -- Keep this scope alive while run-polybar is alive.
            -- On cancellation, withProcessTerm terminates run-polybar
            -- and waits for it to exit.
            exitCode ← ProcT.waitExitCode procHandle
            report $
              "Polybar script "
                <> show polybarRunScript
                <> " finished with: "
                <> show exitCode

      start `E.catch` \(e :: E.SomeException) ->
        report $
          "Polybar script "
            <> show polybarRunScript
            <> " failed: "
            <> E.displayException e

withPolybarReporter
  ∷ IORef.IORef (Maybe (SysIO.Handle, IO () → IO ()))
  → (String, FilePath)
  → ((String → IO ()) → IO a)
  → IO a
withPolybarReporter polybarIpcHandle file@(moduleName, statusStateFile) run = do
  latestRef ← IORef.newIORef ""
  wake ← MVar.newEmptyMVar

  let
    requestUpdate ∷ String → IO ()
    requestUpdate !line = do
      IORef.atomicWriteIORef latestRef line
      void $ MVar.tryPutMVar wake ()

  SysIO.withFile statusStateFile SysIO.WriteMode $ \fileHandle -> do
    SysIO.hSetBuffering fileHandle SysIO.NoBuffering
    Async.withAsync (handler wake latestRef fileHandle) $ \handlerAsync ->
      run requestUpdate `E.finally` Async.cancel handlerAsync

  where
    report = writeFile "/tmp/xmonad-polybar-reporter.log" . (<> "\n")

    handler ∷ MVar.MVar () → IORef.IORef String → SysIO.Handle → IO ()
    handler wake latestRef fileHandle =
      ($ "") . fix $ \again lastWritten → do
        MVar.takeMVar wake
        line ← IORef.readIORef latestRef
        when (lastWritten /= line) (writeStatusAtomic fileHandle line)
        again line

    writeStatusAtomic ∷ SysIO.Handle → String → IO ()
    writeStatusAtomic fileHandle line =
      E.handle @E.SomeException errHandler $ do
        -- Write new line to the state file
        SysIO.hSeek fileHandle SysIO.AbsoluteSeek 0
        SysIO.hPutStrLn fileHandle line
        SysIO.hTell fileHandle >>= SysIO.hSetFileSize fileHandle
        SysIO.hFlush fileHandle

        -- Notify Polybar instances
        IORef.readIORef polybarIpcHandle >>= \case
          Nothing → pure () -- Polybar is not ready yet
          Just (h, withLock) → withLock $ do
            SysIO.hPutStrLn h $ unwords ["TRIGGER_HOOK", moduleName, "0"]
            SysIO.hFlush h
      where
        errHandler (e ∷ E.SomeException) =
          report $
            "Polybar reporter " <> show file <> " failed for line " <>
            show line <> ": " <> E.displayException e

-- * Commands

-- Calling terminals
tmuxedTerminalNew, tmuxedTerminalAttach, tmuxedTerminalNuke, tmuxedTerminalNewPrompt ∷ String
tmuxedTerminalNew = "tmuxed-alacritty-jetbrains-font-dark-new"
tmuxedTerminalAttach = "tmuxed-alacritty-jetbrains-font-dark-attach"
tmuxedTerminalNuke = "tmuxed-alacritty-jetbrains-font-dark-nuke"
tmuxedTerminalNewPrompt = "tmuxed-alacritty-jetbrains-font-dark-new-prompt"

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
cmdRunDark = rofiCmd RofiThemeDark RofiModeRun
cmdRunLight = rofiCmd RofiThemeLight RofiModeRun
cmdDRunDark = rofiCmd RofiThemeDark RofiModeDRun
cmdDRunLight = rofiCmd RofiThemeLight RofiModeDRun

cmdSelectWindowDark, cmdSelectWindowLight ∷ String
cmdSelectWindowDark = rofiCmd RofiThemeDark RofiModeWindow
cmdSelectWindowLight = rofiCmd RofiThemeLight RofiModeWindow

cmdCursorToDisplay ∷ DisplayN → String
cmdCursorToDisplay dn = [qms| place-cursor-at rb {displayNToNum dn ∷ Word} |]

cmdCursorToDisplayAndPlaceAt ∷ DisplayN → String
cmdCursorToDisplayAndPlaceAt dn = [qms| {cmdCursorToDisplay dn} && place-cursor-at |]

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

-- | Complete XMonad configuration
completeConfig ∷ Options → PolybarInterface → XMonad.X () → XConfig _layout
completeConfig opts polybarInterface myStartupHook
  -- Real fullscreen instead of bounding the fullscreen to the window dimentions.
  -- But I personally prefer to keep windows inside their tiles even when they are fullscreened.
  -- N.B. Note that this one must come above “ewmh”.
  -- EwmhDesktops.ewmhFullscreen

  = ManageDocks.docks
  . EwmhDesktops.setEwmhActivateHook UrgencyHook.doAskUrgent
  . EwmhDesktops.ewmh
  . UrgencyHook.withUrgencyHook UrgencyHook.NoUrgencyHook
  . pagerHints
  . withKeysModes opts
  $ myConfig opts polybarInterface myStartupHook

myWorkspaces ∷ [WorkspaceLabel]
myWorkspaces = show <$> [1 .. 10 ∷ Word]

myConfig ∷ Options → PolybarInterface → XMonad.X () → XConfig _layout
myConfig opts polybarInterface myStartupHook = XMonad.def
  { modMask = XMonad.mod4Mask -- Super/Meta/Windows key
  , workspaces = myWorkspaces

  , terminal = "alacritty" -- I don’t really use this value, I have some custom key bindings instead
  , normalBorderColor = "#222222"
  , focusedBorderColor = "#ff0000"
  , focusFollowsMouse = False
  , clickJustFocuses = False

  -- Key/button bindings:

  , keys = defaultModeKeys
  , mouseBindings = myMouseBindings

  -- Hooks:

  , startupHook = myStartupHook
  , layoutHook = layout
  , manageHook = myManageHook
  , logHook = polybarLogHook opts polybarInterface
  , handleEventHook =
      ServerMode.serverModeEventHookF
        xmonadActionXMessageType
        (handleXMonadAction opts polybarInterface layout)
  }
  where
    layout
      -- Workspace 8 has my audio x-over setup
      = onWorkspace "8" (myLayout opts { options_tallMasters = 3 })
      $ myLayout opts

tabsLayoutName, fullscreenLayoutName ∷ String
tabsLayoutName = "Tabs"
fullscreenLayoutName = "Fullscreen"

myLayout ∷ Options → _layout
myLayout opts = go where
  go =
    -- “avoidStruts” to avoid overlapping with the bars
    ManageDocks.avoidStruts (layoutSpacing opts tiles ||| tabbed ||| full)
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
  tall = ResizableTile.ResizableTall opts.options_tallMasters delta ratio []

  -- | Tree-column (or three-row) or grid-like layouts
  grids
    = named "Grid" (GridVariants.SplitGrid GridVariants.L 2 3 (2 / 3) (16 / 10) delta)
    ||| named "Three Columns" (ResizableThreeColumns.ResizableThreeColMid 1 delta ratio [])

  -- | Regular tabs layout where only one window shows at a time
  tabbed = named tabsLayoutName $ Tabbed.tabbed Tabbed.shrinkText myTabTheme

  named name = Renamed.renamed [Renamed.Replace name]

  delta = 1 / 100 -- Resize step
  ratio = 1 / 2 -- Default ratio between the master and the slave windows

  myTabTheme ∷ Tabbed.Theme
  myTabTheme = XMonad.def
    { Tabbed.inactiveColor = _cBg
    , Tabbed.inactiveTextColor = _cFg
    , Tabbed.inactiveBorderColor = _cBorder

    , Tabbed.activeColor = _cBgActive
    , Tabbed.activeTextColor = _cFgActive
    , Tabbed.activeBorderColor = _cBorderActive

    , Tabbed.urgentColor = _cBgUrgent
    , Tabbed.urgentTextColor = _cFgUrgent
    , Tabbed.urgentBorderColor = _cBorderUrgent

    , Tabbed.fontName = "xft:Hack:size=10"
    , Tabbed.decoHeight = 20
    }

-- | Add some space gap to a given layout
layoutSpacing
  ∷ Options
  → layout a
  → ModifiedLayout Renamed.Rename (ModifiedLayout Spacing.Spacing layout) a
layoutSpacing opts
  = Renamed.renamed [Renamed.CutWordsLeft 1] -- Remove “Spacing” prefix
  . Spacing.spacingRaw False (Spacing.Border 0 size 0 size) True (Spacing.Border size 0 size 0) True
  where size = if options_saveMoreSpace opts then 0 else 10

-- ** Mouse bindings

-- | Mouse button mapping set
type MouseButtons = Map.Map (XMonad.KeyMask, XMonad.Button) (XMonad.Window → X ())

-- | Mouse bindings: default actions bound to mouse events
myMouseBindings ∷ XConfig layout → MouseButtons
myMouseBindings XConfig { XMonad.modMask = m } = Map.fromList
  -- %! Set the window to floating mode and move by dragging
  [ ( (m, XMonad.button1)
    , \window → do
        XMonad.focus window
        XMonad.mouseMoveWindow window
        XMonad.windows W.shiftMaster
    )

  -- %! Raise the window to the top of the stack
  , ((m, XMonad.button2), XMonad.windows . (W.shiftMaster .) . W.focusWindow)

  -- %! Set the window to floating mode and resize by dragging
  , ( (m, XMonad.button3)
    , \window → do
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
  operationsShift = [W.shiftMaster, W.swapDown, W.swapUp, shiftLast]
  -- Alternative operations with Shift key pressed.
  -- Swap with currently focused window instead of shifting (just moving) current window.
  operationsSwap = [W.swapMaster, W.swapDown, W.swapUp, swapLast]
  go = mkOperationKeys a operationsShift <> mkOperationKeys s operationsSwap
  mkOperationKeys mod' operations =
    [ ((m .|. mod', key), XMonad.windows operation)
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
  | (workspace, key) ← zip ws $ [XMonad.xK_1 .. XMonad.xK_9] <> [XMonad.xK_0]
  , (mask, operation) ←
      [ (switchMask, switchToWorkspace) -- %! Switch to workspace
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

data MusicPlayerControls = MusicPlayerControls
  { mpPlayCmd ∷ String
  , mpPlayToggleCmd ∷ String
  , mpPrevCmd ∷ String
  , mpNextCmd ∷ String
  , mpStopCmd ∷ String
  , mpSpawnServer ∷ String
  }
  deriving (Eq, Show)

_audaciousMusicPlayerControls ∷ MusicPlayerControls
_audaciousMusicPlayerControls = MusicPlayerControls
  { mpPlayCmd = "audacious --play"
  , mpPlayToggleCmd = "audacious --play-pause"
  , mpPrevCmd = "audacious --rew"
  , mpNextCmd = "audacious --fwd"
  , mpStopCmd = "audacious --stop"
  , mpSpawnServer = "audacious"
  }

mpvcMusicPlayerControls ∷ MusicPlayerControls
mpvcMusicPlayerControls = MusicPlayerControls
  { mpPlayCmd = "mpvc play"
  , mpPlayToggleCmd = "mpvc toggle"
  , mpPrevCmd = "mpvc prev"
  , mpNextCmd = "mpvc next"
  , mpStopCmd = "mpvc stop"
  , mpSpawnServer = tmuxedTerminalNew <> " music mpvc-tui -T"
  }

musicPlayerControlsToKeyBindings ∷ XMonad.KeyMask → MusicPlayerControls → Keys
musicPlayerControlsToKeyBindings m x = Map.fromList
  [ ((m, XF86.xF86XK_AudioPlay), XMonad.spawn x.mpPlayCmd)
  , ((0, XF86.xF86XK_AudioPlay), XMonad.spawn x.mpPlayToggleCmd)
  , ((0, XF86.xF86XK_AudioPrev), XMonad.spawn x.mpPrevCmd)
  , ((0, XF86.xF86XK_AudioNext), XMonad.spawn x.mpNextCmd)
  , ((0, XF86.xF86XK_AudioStop), XMonad.spawn x.mpStopCmd)
  , ((s, XF86.xF86XK_AudioPlay), XMonad.spawn x.mpSpawnServer)
  ]

defaultModeKeys ∷ XConfig XMonad.Layout → Keys
defaultModeKeys
  XConfig
    { XMonad.modMask = m
    , XMonad.workspaces = ws
    , XMonad.layoutHook = layout
    } =
  windowFocusKeys m
  <> windowMoveKeys m
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
      -- Greedy / stealing variants
      , ((m .|. a, XMonad.xK_z), CycleWS.moveTo CycleWS.Prev CycleWS.anyWS)
      , ((m .|. a, XMonad.xK_x), CycleWS.moveTo CycleWS.Next CycleWS.anyWS)
      -- Moving windows to workspaces
      , ((m .|. s, XMonad.xK_z), CycleWS.shiftTo CycleWS.Prev CycleWS.hiddenWS)
      , ((m .|. s, XMonad.xK_x), CycleWS.shiftTo CycleWS.Next CycleWS.hiddenWS)

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
      [ ((m, XMonad.xK_Return), XMonad.spawn tmuxedTerminalNew)
      , ((m .|. a, XMonad.xK_Return), XMonad.spawn tmuxedTerminalAttach)
      , ((m .|. s, XMonad.xK_Return), XMonad.spawn tmuxedTerminalNewPrompt)

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

    mediaKeys = mconcat
      [ makingScreenshots
      , calculator
      , musicPlayerControlsToKeyBindings m mpvcMusicPlayerControls
      , audioControl
      , screenBacklightControl
      ]
      where
        makingScreenshots = Map.fromList
          [ ((0, XMonad.xK_Print), XMonad.spawn "gnome-screenshot")
          , ((m, XMonad.xK_Print), XMonad.spawn "gnome-screenshot -w")
          -- TODO: Test it ↓ This bind was with “--release” flag in my i3wm config.
          , ((0, XMonad.xK_Pause), XMonad.spawn "gnome-screenshot -a")
          , ((m, XMonad.xK_Pause), XMonad.spawn "gnome-screenshot -ia")
          ]

        calculator = Map.fromList
          [ ((0, XF86.xF86XK_Calculator), XMonad.spawn "gnome-calculator")
          ]

        audioControl = Map.fromList
          [ ((m, XF86.xF86XK_AudioMute), XMonad.spawn "pamng reset")
          , ((0, XF86.xF86XK_AudioMute), XMonad.spawn "pamng mute")
          , ((0, XF86.xF86XK_AudioLowerVolume), XMonad.spawn "pamng dec")
          , ((s, XF86.xF86XK_AudioLowerVolume), XMonad.spawn "pamng dec '5.0dB'")
          , ((0, XF86.xF86XK_AudioRaiseVolume), XMonad.spawn "pamng inc")
          , ((s, XF86.xF86XK_AudioRaiseVolume), XMonad.spawn "pamng inc '5.0dB'")
          ]

        screenBacklightControl = Map.fromList
          [ ((0, XF86.xF86XK_MonBrightnessDown), XMonad.spawn "screen-backlight -1%")
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
doKeysMode ∷ XdgRuntimeDir → Modal.Mode
(doKeysModeLabel, doKeysMode) = (label, mode) where
  label = "Do"
  mode xdgRuntimeDir = Modal.mode label $ \XConfig { XMonad.modMask = m } →
    let
      leaveMode = (>> Modal.exitMode)

      cursorToDisplayKeys = Map.fromList
        [ ((mask, key), wrap . XMonad.spawn . cmdCursorToDisplay $ n)
        | (n, key) ← zip
            [minBound .. maxBound ∷ DisplayN]
            [XMonad.xK_z, XMonad.xK_x, XMonad.xK_c, XMonad.xK_v]
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

    , ((m, XMonad.xK_Return), XMonad.spawn tmuxedTerminalNuke >> Modal.exitMode)

    -- Restart XMonad applying the new configuration.
    -- Mnemonic: ‘r’ is for ‘Restart’.
    , ((m, XMonad.xK_r), Modal.exitMode >> restartXMonad Normal Full True)
    -- For dev testing
    , ((m .|. s, XMonad.xK_r), Modal.exitMode >> restartXMonad Dev Full True)
    -- “Shallow” mode restart (skipping the autostart script)
    -- Mnemonic: ‘t’ is for ‘restarT’.
    , ((m, XMonad.xK_t), Modal.exitMode >> restartXMonad Normal Shallow True)
    -- For dev testing
    , ((m .|. s, XMonad.xK_t), Modal.exitMode >> restartXMonad Dev Shallow True)
    -- Exit XMonad. Log out from the X session.
    -- Mnemonic: ‘e’ is for ‘Exit’.
    , ((m, XMonad.xK_e), exitXMonadPrompt xdgRuntimeDir >> Modal.exitMode)

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
withKeysModes ∷ Options → XConfig layout → XConfig layout
withKeysModes opts = HModal.modal . mconcat $
  [ [doKeysMode opts.options_xdgRuntimeDir, workspaceKeysMode, mouseCursorKeysMode]
  , [resizeKeysMode x | x ← [minBound .. maxBound]]
  , [positioningKeysMode x | x ← [minBound .. maxBound]]
  ]

-- * Manage hook

myManageHook ∷ XMonad.ManageHook
myManageHook = XMonad.composeAll
  [ XMonad.className =? "Gpaste-gui" --> doCenterFloat
  , XMonad.className =? "gnome-calculator" --> doCenterFloat

  -- Audio apps
  , XMonad.className =? "calfjackhost" --> doTile
  , XMonad.className =? "Jalv.gtk" --> doTile
  -- This tuner app does not render properly when
  -- window is resized to anything non-standard
  , XMonad.title =? "x42 Instrument Tuner" --> doCenterFloat

  -- Moving to last workspace
  , XMonad.className =? "thunderbird" --> moveTo imWsLabel
  , XMonad.className =? "nheko" --> moveTo imWsLabel
  , XMonad.className =? "Psi+" --> moveTo imWsLabel
  , XMonad.className =? "dino" --> moveTo imWsLabel
  , XMonad.className =? "Hexchat" --> moveTo imWsLabel

  , isAbove --> XMonad.doFloat
  , isStickyWindow --> XMonad.doF copyToAll

  , alterInsertPosition
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
      atom ← XMonad.getAtom "_NET_WM_STATE"
      maybe [] (fmap fromIntegral) <$> getProp32 atom window

    hasNetWMState ∷ String → XMonad.Window → X Bool
    hasNetWMState state window = elem <$> XMonad.getAtom state <*> getNetWMState window

    isAbove ∷ XMonad.Query Bool
    isAbove = XMonad.liftX . hasNetWMState "_NET_WM_STATE_ABOVE" =<< XMonad.ask

    isStickyWindow ∷ XMonad.Query Bool
    isStickyWindow = XMonad.liftX . hasNetWMState "_NET_WM_STATE_STICKY" =<< XMonad.ask

    alterInsertPosition =
      -- Condition only to non-floating window.
      -- Without the condition a call for “place-cursor-at” for example will switch to a next window.
      -- For some reason GPaste GUI does not show up as floating one, though it definitely is.
      conditionMonad -->
        -- Make new windows appear after the window under focus (not at the place of focused window).
        InsertPosition.insertPosition InsertPosition.Below InsertPosition.Newer
      where
        conditionMonad =
          fmap not XMonad.willFloat
          <&&> fmap (/= "Gpaste-gui") XMonad.className
          <&&> fmap (/= "gnome-calculator") XMonad.className

-- * Helpers

data RofiMode
  = RofiModeRun
  | RofiModeDRun
  | RofiModeWindow
  | RofiModeDMenu RofiDmenuOptions

data RofiDmenuOptions = RofiDmenuOptions
  { rofiDmenu_prompt ∷ String
  , rofiDmenu_strict ∷ Bool -- Do not allow arbitrary text (only provided options)
  , rofiDmenu_selectRow ∷ Maybe Word
  }

data RofiTheme = RofiThemeDark | RofiThemeLight
rofiThemeForCmd ∷ RofiTheme → String
rofiThemeForCmd = \case RofiThemeDark → "gruvbox-dark" ; RofiThemeLight → "gruvbox-light-soft"

rofiCmd ∷ RofiTheme → RofiMode → String
rofiCmd theme mode =
  unwords . mconcat $
    [ ["rofi", "-theme", rofiThemeForCmd theme]
    , case mode of
        RofiModeRun → ["-show", "run"]
        RofiModeDRun → ["-show", "drun"]
        RofiModeWindow → ["-show", "window"]
        RofiModeDMenu opts → mconcat
          [ ["-dmenu", "-matching", "fuzzy", "-case-smart"]
          , if opts.rofiDmenu_strict then ["-no-custom"] else mempty
          , maybe mempty (\x → ["-select-row", show x]) opts.rofiDmenu_selectRow
          , ["-p", shellQuote opts.rofiDmenu_prompt]
          ]
    ]

exitXMonadPrompt ∷ XdgRuntimeDir → X ()
exitXMonadPrompt xdgRuntimeDir = do
  withFifoResponse xdgRuntimeDir "exit-monad-prompt" $ \fifoPath getFifoLine → do
    XMonad.spawn [qmb|
      yes='Yes, terminate XMonad'
      options="$yes"$'\nCancel\n'
      answer=$(<<<"$options" {prompt}) || true

      # Report NO response.
      # It’s okay if this comes after YES, it won’t change the outcome.
      finish() \{ echo >> {shellQuote fifoPath}; }
      trap finish EXIT

      if [[ "$answer" == "$yes" ]]; then
        echo 1 >> {shellQuote fifoPath}
      fi
    |]

    -- Do not set a timeout, because the prompt window can be shown
    -- for undefined time.
    getFifoLine Nothing (pure . \case Right "1" → True;  _ → False) >>=
      bool (pure ()) exitXMonad

  where
    prompt =
      rofiCmd RofiThemeDark . RofiModeDMenu $ RofiDmenuOptions
        { rofiDmenu_prompt =
            "Do you really want to terminate XMonad? This will end your X session!"
        , rofiDmenu_strict = True
        , rofiDmenu_selectRow = Just 1
        }

exitXMonad ∷ XMonad.MonadIO io ⇒ io ()
exitXMonad = XMonad.io $
  PosixProc.getProcessID >>= PosixSignals.signalProcess PosixSignals.sigTERM

data XMonadStartMode
  = Full -- ^ Regular full (re)start of XMonad ((re)load fresh config)
  | Shallow -- ^ Skip autostart script on (re)start
  deriving (Show, Read, Eq, Enum, Bounded)

instance XMonad.Default XMonadStartMode where def = Full

data XMonadExecutableType
  = Normal
  | Dev
  deriving (Show, Read, Eq, Enum, Bounded)

xmonadExecutable ∷ XMonadExecutableType → String
xmonadExecutable = \case
  Normal → "xmonad"
  Dev → "/etc/nixos/gui/xmonad/xmonad-dev.sh"
{-# INLINE xmonadExecutable #-}

-- | Customized "XMonad.Operations.restart" that also passes “start mode” environment variable
--
-- See "startModeMarkerEnvName" for environment variable name.
-- See "XMonadStartMode" for possible environment variable values (it’s just “show”n).
restartXMonad ∷ XMonadExecutableType → XMonadStartMode → Bool → X ()
restartXMonad xmonadExecutableType mode resume = do
  XMonad.broadcastMessage XMonad.ReleaseResources
  XMonad.io . XMonad.flush =<< XMonad.asks XMonad.display
  when resume XMonad.writeStateToFile
  XMonad.catchIO $ do
    env ← (<> [startModeMarker]) . filter ((/= startModeMarkerEnvName) . fst) <$> getEnvironment
    executeFile (xmonadExecutable xmonadExecutableType) True [] (Just env)
  where
    startModeMarker = (startModeMarkerEnvName, show mode)

-- | Environment variable name
startModeMarkerEnvName ∷ String
startModeMarkerEnvName = "WENZELS_XMONAD_START_MODE"

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
focusLast ∷ W.StackSet i l XMonad.Window s sd → W.StackSet i l XMonad.Window s sd
focusLast = W.modify' $ \c → case c of
  W.Stack t ls (NE.nonEmpty → fmap NE.reverse → Just (lastWindow NE.:| rs)) →
    W.Stack lastWindow (rs <> (t : ls)) []
  _ → c -- Already last

-- | Move focused window to the end (to become the last one)
--
-- Kind of the opposite of "W.shiftMaster".
shiftLast ∷ W.StackSet i l a s sd → W.StackSet i l a s sd
shiftLast = W.modify' $ \c → case c of
  W.Stack _ _ [] → c -- Already last
  W.Stack t ls rs → W.Stack t (reverse rs <> ls) []

-- | Swap focused and the last window’s positions
--
-- Kind of the opposite of "W.swapMaster".
swapLast ∷  W.StackSet i l a s sd → W.StackSet i l a s sd
swapLast = W.modify' $ \c → case c of
  W.Stack t ls (NE.nonEmpty → fmap NE.reverse → Just (lastWindow NE.:| rs)) →
    W.Stack t (rs <> (lastWindow : ls)) []
  _ → c -- Already last

-- Either @W.view@ or @W.greedyView@
switchToWorkspace
  ∷ (Eq s, Eq workspaceLabel)
  ⇒ workspaceLabel
  → W.StackSet workspaceLabel l a s sd
  → W.StackSet workspaceLabel l a s sd
switchToWorkspace = W.greedyView
{-# INLINE switchToWorkspace #-}

polybarWindowFlags ∷ XMonadExecutableType → XMonad.X String
polybarWindowFlags executableType =
  XMonad.gets (W.peek . XMonad.windowset) >>= \case
    Nothing → pure ""
    Just window → do
      isSticky ← TagWindows.hasTag stickyWindowTag window
      isFloating ← isWindowFloating window
      let disabled = pbBg _cBorderUrgent
      pure $ intercalate (pbPad "4px" "")
        [ pbLMB executableType (xMonadActionToAtomString ToggleStickyWindow) $
          (if isSticky then pbFgBg _cFgUrgent _cBgUrgent else disabled)
          (pbPad' (if isSticky then "\984067" else "\985393"))
        , pbLMB executableType (xMonadActionToAtomString ToggleFloatingWindow) $
          (if isFloating then pbFgBg _cFgActive _cBgActive else disabled)
          (pbPad' (if isFloating then "\983624" else "\9633"))
        ]

handleXMonadAction
  ∷ (Read (layout XMonad.Window), XMonad.LayoutClass layout XMonad.Window)
  ⇒ Options
  → PolybarInterface
  → layout XMonad.Window
  → String
  → XMonad.X ()
handleXMonadAction opts polybarInterface layout actionStr =
  case parseXMonadAction actionStr of
    Nothing →
      XMonad.io . writeFile "/tmp/xmonad-action-handler.log" . (<> "\n") $
        "Unrecognized XMonad action: " <> show actionStr
    Just action →
      case action of
        ToggleStickyWindow → do
          toggleStickyWindow
          -- Report the flags, otherwise toggled sticky status
          -- does not propagate to Polybar. @logHook@ for some
          -- reason is not handling it for this particular case.
          polybarWindowFlags opts.options_executableType
            >>= XMonad.io . polybarInterface.polybar_reportWindowFlags
        ToggleFloatingWindow → XMonad.withFocused toggleFloatWindow
        CycleLayout → XMonad.sendMessage XMonad.NextLayout
        ResetLayout → XMonad.setLayout (XMonad.Layout layout)
        ResetMode → Modal.exitMode
        SwitchWorkspace ws → XMonad.windows (switchToWorkspace ws)

polybarLogHook ∷ Options → PolybarInterface → XMonad.X ()
polybarLogHook opts polybarInterface = do
  workspacesPP
    >>= DynamicLog.dynamicLogString
    >>= XMonad.io . polybarInterface.polybar_reportWorkspaces

  DynamicLog.dynamicLogString layoutPP
    >>= XMonad.io . polybarInterface.polybar_reportLayout

  DynamicLog.dynamicLogString modePP
    >>= XMonad.io . polybarInterface.polybar_reportMode

  polybarWindowFlags et
    >>= XMonad.io . polybarInterface.polybar_reportWindowFlags

  where
    workspacesPP ∷ XMonad.X DynamicLog.PP
    workspacesPP = do
      visibleOn ← workspaceScreenMap
      let f = formatWorkspace visibleOn

      pure $ XMonad.def
        { DynamicLog.ppCurrent = \w →
            pbLMB et (xMonadActionToAtomString $ SwitchWorkspace w) $
            pbWorkspace (Just _cFgActive) (Just _cBgActive) (f w)
        , DynamicLog.ppVisible = \w →
            pbLMB et (xMonadActionToAtomString $ SwitchWorkspace w) $
            pbWorkspace Nothing (Just _cBorderUrgent) (f w)
        , DynamicLog.ppHidden = \w →
            pbLMB et (xMonadActionToAtomString $ SwitchWorkspace w) $
            pbWorkspace Nothing Nothing (f w)
        , DynamicLog.ppHiddenNoWindows = \w →
            pbLMB et (xMonadActionToAtomString $ SwitchWorkspace w) $
            pbWorkspace (Just _cFgDisabled) Nothing (f w)
        , DynamicLog.ppUrgent = \w ->
            pbLMB et (xMonadActionToAtomString $ SwitchWorkspace w) $
            pbWorkspace (Just _cFgUrgent) (Just _cBgUrgent) (f w)
        , DynamicLog.ppLayout = const mempty
        , DynamicLog.ppTitle = const mempty
        , DynamicLog.ppWsSep = mempty
        }

    layoutPP ∷ DynamicLog.PP
    layoutPP =
      XMonad.def
        { DynamicLog.ppCurrent = const mempty
        , DynamicLog.ppVisible = const mempty
        , DynamicLog.ppHidden = const mempty
        , DynamicLog.ppHiddenNoWindows = const mempty
        , DynamicLog.ppUrgent = const mempty
        , DynamicLog.ppLayout = \layout →
            pbLMB et (xMonadActionToAtomString CycleLayout) $
            pbRMB et (xMonadActionToAtomString ResetLayout) $
            layout
        , DynamicLog.ppTitle = const mempty
        , DynamicLog.ppWsSep = mempty
        }

    modePP ∷ DynamicLog.PP
    modePP =
      XMonad.def
        { DynamicLog.ppCurrent = const mempty
        , DynamicLog.ppVisible = const mempty
        , DynamicLog.ppHidden = const mempty
        , DynamicLog.ppHiddenNoWindows = const mempty
        , DynamicLog.ppUrgent = const mempty
        , DynamicLog.ppLayout = const mempty
        , DynamicLog.ppTitle = const mempty
        , DynamicLog.ppExtras = [modeLogger]
        , DynamicLog.ppWsSep = mempty
        }

    screenIdToInt ∷ XMonad.ScreenId → Int
    screenIdToInt (XMonad.S i) = i

    workspaceScreenMap ∷ XMonad.X (Map.Map XMonad.WorkspaceId Int)
    workspaceScreenMap = XMonad.withWindowSet $ \ws →
      pure . Map.fromList $
        [ (W.tag (W.workspace screen), screenIdToInt (W.screen screen) + 1)
        | screen ← W.current ws : W.visible ws
        ]

    formatWorkspace ∷ Map.Map XMonad.WorkspaceId Int → XMonad.WorkspaceId → String
    formatWorkspace visibleOn tag =
      maybe tag (\screenNo → tag <> formatScreenNo screenNo) (Map.lookup tag visibleOn)
      where formatScreenNo = \case 1 → "-"; 2 → "="; 3 → "≡"; 4 → "≢"; _ → "_"

    modeLogger ∷ Logger
    modeLogger =
      Modal.logMode >>=
        pure . Just . maybe "Normal"
          ( pbLMB et (xMonadActionToAtomString ResetMode)
          . pbFgBg _cFgUrgent _cBgUrgent
          . pbPad'
          )

    et = opts.options_executableType

pbAction ∷ Word → XMonadExecutableType → String → String → String
pbAction button executableType ctlCommand text = mconcat
  [ "%{A", show button, ":"
  , xmonadExecutable executableType, " ctl ", ctlCommand
  , ":}", text, "%{A}"
  ]

-- Left mouse button
pbLMB ∷ XMonadExecutableType → String → String → String
pbLMB = pbAction 1
{-# INLINE pbLMB #-}

-- Right mouse button
pbRMB ∷ XMonadExecutableType → String → String → String
pbRMB = pbAction 3
{-# INLINE pbRMB #-}

-- Duplicating colors from Polybar config.
_cBg, _cFg, _cBorder
  , _cBgActive, _cFgActive, _cBorderActive
  , _cBgUrgent, _cFgUrgent, _cBorderUrgent
  , _cFgDisabled
  ∷ String
-- Normal
_cBg = "#222"
_cFg = "#888"
_cBorder = "#333"
-- Active
_cBgActive = "#285577"
_cFgActive = "#ffffff"
_cBorderActive = "#4c7899"
-- Urgent
_cBgUrgent = "#900000"
_cFgUrgent = "#fff"
_cBorderUrgent = "#2f343a"
-- Disabled
_cFgDisabled = "#555"
{-# INLINE _cBg #-}
{-# INLINE _cFg #-}
{-# INLINE _cBorder #-}
{-# INLINE _cBgActive #-}
{-# INLINE _cFgActive #-}
{-# INLINE _cBorderActive #-}
{-# INLINE _cBgUrgent #-}
{-# INLINE _cFgUrgent #-}
{-# INLINE _cBorderUrgent #-}
{-# INLINE _cFgDisabled #-}

pbFg ∷ String → String → String
pbFg color text = mconcat ["%{F", color, "}", text, "%{F-}"]

pbBg ∷ String → String → String
pbBg color text = mconcat ["%{B", color, "}", text, "%{B-}"]

pbFgBg ∷ String → String → String → String
pbFgBg fg bg text = mconcat ["%{F", fg, "}%{B", bg, "}", text, "%{B-}%{F-}"]

pbPad ∷ String → String → String
pbPad amount text = mconcat ["%{O", amount, "}", text, "%{O", amount, "}"]

pbPad' ∷ String → String
pbPad' = pbPad "8px"
{-# INLINE pbPad' #-}

pbWorkspace ∷ Maybe String → Maybe String → String → String
pbWorkspace mFg mBg name = applyFgBg (pbPad' name)
  where
    applyFgBg =
      case (mFg, mBg) of
        (Just fg, Just bg) → pbFgBg fg bg
        (Just fg, Nothing) → pbFg fg
        (Nothing, Just bg) → pbBg bg
        (Nothing, Nothing) → id

shellQuote ∷ String → String
shellQuote str = "'" <> concatMap (\case '\'' → "'\\''"; c → [c]) str <> "'"
{-# INLINE shellQuote #-}

type Microseconds = Int

type WithReadFifoLine b
  = Maybe Microseconds
  -- ^ Optional timeout (to prevent a read blocking indefinitely)
  → (Either E.SomeException String → IO b)
  -- ^ FIFO line read result handler
  --   (you can check "E.SomeException" for "FifoResponseTimedOut" if you
  --   set the timeout)
  → X b

-- | Seconds to "Microseconds"
_seconds ∷ Int → Microseconds
_seconds = (* 1_000_000)
{-# INLINE _seconds #-}

-- | Milliseconds to "Microseconds"
_ms ∷ Int → Microseconds
_ms = (* 1_000)
{-# INLINE _ms #-}

-- | Create a FIFO file and provide its path to the callback and a function to
--   read a line from that FIFO file in blocking mode
--
-- XMonad does something weird to the sub-processes (maybe `waitpid(-1, …)` or
-- something like that). Sometimes sub-processes you create can fail due to
-- that during normal operation. So trying to create a process and read from it
-- can give you issues. This helper can help with that using an alternative
-- approach. If you need to wait for a script to finish you "XMonad.spawn" and
-- forget a shell script giving it the path to the created FIFO to report when
-- it’s done. And then you wait for that report from the FIFO by using provided
-- callback to read a line from FIFO. Thus you don’t need to track the
-- sub-process life time directly, you don’t have to wait for its termination,
-- etc.
--
-- Note that if your sub-process/script fails to report to the FIFO the reader
-- function will block indefinitely. It is up to you how to handle this.
withFifoResponse
  ∷ XdgRuntimeDir
  → String
  → (FilePath → WithReadFifoLine b → X a)
  -- ^ FIFO file path and "WithReadFifoLine" callback which
  --   blocks and waits for line to be written to FIFO
  → X a
withFifoResponse xdgRuntimeDir fifoNameSuffix m = do
  fifoPath ← XMonad.io mkFifoPath
  flip finallyX (XMonad.io $ removeFileIfExists fifoPath) $ do
    XMonad.io (mkRuntimeFifo fifoPath)
    m fifoPath (\timeoutMay → XMonad.io . withReadFifoLine fifoPath timeoutMay)
  where
    -- Note that "SysIO.ReadWriteMode" is intentional instead of
    -- "SysIO.ReadMode" (otherwise it’s failing).
    readFifoLine fifoPath =
      SysIO.withFile fifoPath SysIO.ReadWriteMode SysIO.hGetLine

    withReadFifoLine fifoPath timeoutMay =
      (E.try (timeout fifoPath timeoutMay $ readFifoLine fifoPath) >>=)

    timeout fifoPath =
      maybe id $ \t m' →
        Timeout.timeout t m' >>=
          maybe (E.throwIO $ FifoResponseTimedOut fifoPath t) pure

    removeFileIfExists ∷ FilePath → IO ()
    removeFileIfExists path = do
      E.catch (Dir.removeFile path) $ \(e ∷ IOError.IOError) →
        if IOError.isDoesNotExistError e then pure () else E.throwIO e

    mkFifoPath ∷ IO FilePath
    mkFifoPath = do
      -- XMonad PID stays the same between restarts, so using time.
      time ∷ Word ← round . (* 1_000_000_000) <$> POSIX.getPOSIXTime
      PosixProc.getProcessID <&> \pid →
        xdgRuntimeDir.unXdgRuntimeDir </> mconcat
          ["xmonad-", show pid, "-", show time, "-", fifoNameSuffix, ".fifo"]

    mkRuntimeFifo ∷ FilePath → IO ()
    mkRuntimeFifo fifoPath = do
      -- Should not exist because name is unique,
      -- but this makes retries harmless.
      removeFileIfExists fifoPath
      PosixFiles.createNamedPipe fifoPath PosixFiles.ownerModes

data FifoResponseTimedOut = FifoResponseTimedOut
  { fifoResponsePath ∷ FilePath
  , fifoResponseTimeout ∷ Microseconds
  }
  deriving (Eq, Show)

instance E.Exception FifoResponseTimedOut where
  displayException (FifoResponseTimedOut path t) = unwords
    [ "Timed out waiting for FIFO response from"
    , show path, "after", show t, "μs"
    ]

-- | Async-exception safer "E.finally" for "X" monad using only public XMonad API
--
-- The main action runs with async exceptions restored.
-- Cleanup runs under mask.
-- If cleanup throws, cleanup's exception wins, like "E.finally".
finallyX ∷ X a → X b → X a
finallyX m final = do
  st0 ← XMonad.get
  c ← XMonad.ask

  (result, stFinal) ← XMonad.io $
    E.mask $ \restore →
      E.try @E.SomeException (restore $ XMonad.runX c st0 m) >>= \case
        Right (x, st1) →
          E.try @E.SomeException (XMonad.runX c st1 final) <&> \case
            Right (_, st2) → (Right x, st2)
            -- Main succeeded, cleanup failed:
            -- throw cleanup exception, state remains after main action.
            Left finalException → (Left finalException, st1)

        Left mainException →
          E.try @E.SomeException (XMonad.runX c st0 final) <&> \case
            -- Main failed, cleanup succeeded: rethrow original exception,
            -- but commit cleanup state.
            Right (_, st2) → (Left mainException, st2)
            -- Main failed, cleanup failed:
            -- cleanup exception wins, original state remains.
            Left finalException → (Left finalException, st0)

  XMonad.put stFinal
  either (XMonad.io . E.throwIO) pure result
