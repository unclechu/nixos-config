-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE UnicodeSyntax, GHC2024, QuasiQuotes, OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

-- | Main application runner module
module WenzelsI3StatusGenerator.App
     ( runApp
     ) where

import Control.Arrow ((&&&))
import qualified Control.Concurrent as CC
import qualified Control.Concurrent.Async as Async
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception as E
import Control.Monad (join, guard)
import Data.Aeson (ToJSON (..), genericToJSON, encode)
import Data.Default (Default (def))
import Data.Foldable (forM_)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import Foreign.C.Types (CInt)
import GHC.Generics (Generic)
import Graphics.X11.Xlib (openDisplay, closeDisplay)
import Prelude hiding (log)
import System.Environment (getArgs)
import qualified System.IO as SysIO
import qualified System.Posix.Signals as Sig
import Text.InterpolatedString.QM (qm)
import WenzelsI3StatusGenerator.Dzen (dzen)
import WenzelsI3StatusGenerator.EventSubscriber.Battery (subscribeToBatteryChargeUpdates)
import WenzelsI3StatusGenerator.EventSubscriber.DateTime (subscribeToDateTimeUpdates)
import WenzelsI3StatusGenerator.EventSubscriber.InputEvents (subscribeToClickEvents)
import WenzelsI3StatusGenerator.EventSubscriber.IPC (IPCEvent (..), subscribeToIPCEvents)
import qualified WenzelsI3StatusGenerator.EventSubscriber.WindowTitle as WindowTitle
import WenzelsI3StatusGenerator.Handler.AppState (State (..), appStateHandler)
import qualified WenzelsI3StatusGenerator.Handler.InputEvents as InputEvents
import WenzelsI3StatusGenerator.IPC (ipcSwitchAlternativeModeSignal)
import WenzelsI3StatusGenerator.ParentProc (dieWithParent)
import WenzelsI3StatusGenerator.Utils (getDisplayName, ($>), (∘), (<&>), (•), echo, (↔), (≠))
import WenzelsI3StatusGenerator.Utils.Aeson (withFieldNamer)
import WenzelsI3StatusGenerator.X (initThreads)


runApp ∷ IO ()
runApp = do
  log ← mkStderrLogger

  getArgs >>= \case
    [] → pure ()
    args → fail $
      "This applictaion does not expect any arguments but got: " <> show args

  initThreads

  withDisplayMarker ← do
    dpy ← openDisplay ""
    let !x = getDisplayName dpy
    closeDisplay dpy $> WithDisplayMarker ($ x)

  (putStateModification, getNextStateModification) ←
    (MVar.putMVar &&& MVar.takeMVar) <$> MVar.newEmptyMVar

  (ipcEmitSignal, ipcEventsThreadHandle) ←
    subscribeToIPCEvents (unWithDisplayMarker withDisplayMarker) $ \case
      NumLock x → putStateModification $ \s → s { numLock = x }
      CapsLock x → putStateModification $ \s → s { capsLock = x }
      KbdLayout x → putStateModification $ \s → s { kbdLayout = Just x }
      Alternative x → putStateModification $ \s → s { alternative = x }

  (initialDateAndTime, dateAndTimeThreadHandle) ←
    subscribeToDateTimeUpdates $ \(utc, timeZone) →
      putStateModification $ \s → s { lastTime = Just (utc, timeZone) }

  !batteryChargeUpdatesSubscription ←
    subscribeToBatteryChargeUpdates $ \case
      (Nothing, Nothing) → pure ()

      (Just chargeLeft, Just chargeState) →
        putStateModification $ \s → s
          { battery = Just (chargeLeft, chargeState) }

      (Just chargeLeft, Nothing) →
        putStateModification $ \s → s
          { battery = battery s >>= Just ∘ (chargeLeft,) ∘ snd }

      (Nothing, Just chargeState) →
        putStateModification $ \s → s
          { battery = battery s >>= Just ∘ (,chargeState) ∘ fst }

  (initialFocusedWindowTitle, focusedWindowTitleThreadHandle) ←
    WindowTitle.subscribeToFocusedWindowTitleUpdates $
      \x → putStateModification $ \s → s { windowTitle = x <&> (.unWindowTitle) }

  (saveState, readState) ←
    (writeIORef &&& readIORef) <$> newIORef def
      { battery = fst <$> batteryChargeUpdatesSubscription
      , windowTitle = initialFocusedWindowTitle <&> (.unWindowTitle)
      , lastTime = Just initialDateAndTime
      }

  clickEventsThreadHandle
    ← subscribeToClickEvents
    . InputEvents.handleClickEvent
    $ InputEvents.HandleClickEventInterface
    { alternativeModeClickHandler =
        let
          !newState =
            readState <&> alternative • \case
              Nothing     → 1
              Just (1, _) → 2
              _           → 0
          signal =
            ipcSwitchAlternativeModeSignal
              (unWithDisplayMarker withDisplayMarker)
        in
          ipcEmitSignal ∘ signal =<< newState

    , getCurrentKbdLayout
        = readState <&> kbdLayout
        • fmap (either (const Nothing) Just) • join
    }

  dieWithParent -- Make this app die if the parent process (i3 bar) dies

  echo $ encode (def ∷ ProtocolInitialization) { clickEvents = True }

  appStateThreadHandle ←
    appStateHandler dzen getNextStateModification saveState =<< readState

  -- Handle POSIX signals to terminate application
  threadHandles ∷ [(IO (), Async.Async ())] ←
    mapM (uncurry $ withTerminationReport log)
      [ ("focusedWindowTitleThreadHandle", focusedWindowTitleThreadHandle)
      , ("ipcEventsThreadHandle", ipcEventsThreadHandle)
      , ("dateAndTimeThreadHandle", dateAndTimeThreadHandle)
      , ("clickEventsThreadHandle", clickEventsThreadHandle)
      , ("appStateThreadHandle", appStateThreadHandle)
      ]

  let
    terminateApplication ∷ Maybe TerminationSig → IO ()
    terminateApplication sig =
      go `E.catch` \(e ∷ E.SomeException) →
        log ∘ unwords $
          [ "terminateApplication for"
          , maybe "main thread" (("for " ↔) ∘ show) sig
          , "threw exception:", E.displayException e
          ]
      where
        go = do
          log $
            case sig of
              Nothing → "Main thread triggered termination"
              Just sig' → unwords [show sig', "signal triggered termination"]
          foldl' E.finally (pure ()) $
            [maybe (pure ()) snd batteryChargeUpdatesSubscription]
            ↔ fmap fst threadHandles

  forM_ [minBound .. maxBound] $ \sig →
    Sig.installHandler
      (toSigNum sig)
      ((Sig.Catch ∘ terminateApplication ∘ Just) sig)
      Nothing

  do -- Hanlding termination of the application

    Async.waitAnyCatch (threadHandles <&> snd) >>= \(thread, result) →
      log ∘ unwords $
        [ "Main thread termination: Thread watcher"
        , "(" ↔ (show ∘ Async.asyncThreadId) thread ↔ ")"
        , "reported:"
        ]
        ↔ case result of
            Right () → ["successful exit"]
            Left (E.fromException → Just Async.AsyncCancelled) → ["cancellation"]
            Left e → ["failure:", E.displayException e]

    -- Send cancellation signal to all other threads.
    -- And also call unsubscriber functions.
    terminateApplication Nothing

    -- Wait each thread individually and collect information about exceptions
    terminationExceptions ←
      fmap catMaybes ∘ Async.forConcurrently threadHandles $ \(_, asyncHandle) →
        Async.waitCatch asyncHandle
          <&> either Just (const Nothing)
          • (>>= \e → e <$ guard (E.fromException e ≠ Just Async.AsyncCancelled))
          • fmap (Async.asyncThreadId asyncHandle,)

    if null terminationExceptions
    then pure () -- Normal successful exit
    else
      fail ∘ intercalate "\n\n" $ terminationExceptions <&> \(tid, e) →
        [qm| Thread ({tid}) has failed with exception: {E.displayException e} |]


withTerminationReport
  ∷ Logger
  → String
  → Async.Async ()
  → IO (IO (), Async.Async ())
withTerminationReport log threadName watchedThread = do
  watcherThread ← Async.async $
    Async.wait watchedThread `E.catch` \(e ∷ E.SomeException) → do
      threadId ← CC.myThreadId
      log ∘ unwords $
        [ "Thread", show threadName, "watcher", "(" ↔ show threadId ↔ "):"
        , "Thread", show threadName, "(" ↔ (show ∘ Async.asyncThreadId) watchedThread ↔ ")"]
        ↔ case E.fromException e of
            Just Async.AsyncCancelled → ["was cancelled"]
            Nothing → ["failed with:", E.displayException e]
      E.throwIO e
  pure (Async.uninterruptibleCancel watchedThread, watcherThread)


data TerminationSig = SIGHUP | SIGINT | SIGTERM | SIGPIPE
  deriving stock (Eq, Show, Bounded, Enum)

toSigNum ∷ TerminationSig → CInt
toSigNum = \case
  SIGHUP → Sig.sigHUP
  SIGINT → Sig.sigINT
  SIGTERM → Sig.sigTERM
  SIGPIPE → Sig.sigPIPE
{-# INLINE toSigNum #-}


type Logger = String → IO ()

mkStderrLogger ∷ IO Logger
mkStderrLogger = do
  lock ← MVar.newMVar ()
  pure $ \msg →
    MVar.withMVar lock $ \() → do
      SysIO.hPutStrLn SysIO.stderr msg
      SysIO.hFlush SysIO.stderr


-- * Types

data ProtocolInitialization
  = ProtocolInitialization
  { version ∷ Word
  , stopSignal ∷ Maybe Int
  , contSignal ∷ Maybe Int
  , clickEvents ∷ Bool
  }
  deriving (Show, Eq, Generic)

instance Default ProtocolInitialization where
  def
    = ProtocolInitialization
    { version = 1
    , stopSignal = Nothing
    , contSignal = Nothing
    , clickEvents = False
    }

instance ToJSON ProtocolInitialization where
  toJSON = genericToJSON $ withFieldNamer id


-- | A wrapper to avoid impredicative polymorphism limitation
newtype WithDisplayMarker
  = WithDisplayMarker
  { unWithDisplayMarker ∷ ∀a. (String → a) → a
  }
