-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE UnicodeSyntax, GHC2024, QuasiQuotes, OverloadedRecordDot #-}

-- | Main application runner module
module WenzelsI3StatusGenerator.App
     ( runApp
     ) where

import Control.Arrow ((&&&))
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (finally, fromException, displayException)
import Control.Monad (join, guard)
import Data.Aeson (ToJSON (..), genericToJSON, encode)
import Data.Default (Default (def))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (intercalate)
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Graphics.X11.Xlib (openDisplay, closeDisplay)
import System.Environment (getArgs)
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
import WenzelsI3StatusGenerator.Utils
import WenzelsI3StatusGenerator.Utils.Aeson (withFieldNamer)
import WenzelsI3StatusGenerator.X (initThreads)


runApp ∷ IO ()
runApp = do
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
    (putMVar &&& takeMVar) <$> newEmptyMVar

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
            readState <&> alternative <&> \case
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

  dzenNotification
    ← newIORef Nothing <&>
    \ ref text color → fireAndForget $ dzen ref text color

  echo $ encode (def ∷ ProtocolInitialization) { clickEvents = True }

  appStateThreadHandle ←
    appStateHandler dzenNotification getNextStateModification saveState =<< readState

  -- Handle POSIX signals to terminate application
  let
    threadHandles =
      [ focusedWindowTitleThreadHandle
      , ipcEventsThreadHandle
      , dateAndTimeThreadHandle
      , clickEventsThreadHandle
      , appStateThreadHandle
      ]

    terminateApplication
      = foldl' finally (pure ())
      $ [maybe (pure ()) snd batteryChargeUpdatesSubscription]
      ⋄ fmap Async.uninterruptibleCancel threadHandles

  mapM_
    (\sig → Sig.installHandler sig (Sig.Catch terminateApplication) Nothing)
    [Sig.sigHUP, Sig.sigINT, Sig.sigTERM, Sig.sigPIPE]

  do -- Hanlding termination of the application

    _ ← Async.waitAnyCatch threadHandles

    -- Send cancellation signal to all other threads.
    -- And also call unsubscriber functions.
    terminateApplication

    -- Wait each thread individually and collect information about exceptions
    terminationExceptions ←
      fmap catMaybes ∘ Async.forConcurrently threadHandles $ \asyncHandle →
        Async.waitCatch asyncHandle
          <&> either Just (const Nothing)
          <&> (>>= \e → e <$ guard (fromException e ≠ Just Async.AsyncCancelled))
          <&> fmap (Async.asyncThreadId asyncHandle,)

    if null terminationExceptions
    then pure () -- Normal successful exit
    else fail ∘ intercalate "\n\n" $ terminationExceptions <&> \(tid, e) →
           [qm| Thread ({tid}) has failed with exception: {displayException e} |]


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
