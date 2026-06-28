-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE UnicodeSyntax, GHC2024 #-}

-- | Dzen2 notification window spawning functions
module WenzelsI3StatusGenerator.Dzen
     ( dzen
     ) where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as Async
import qualified Data.IORef as IORef
import System.IO (Handle, hPutStrLn, hFlush, hClose)
import System.Mem.Weak (addFinalizer)
import qualified System.Process as SysProc
import WenzelsI3StatusGenerator.Utils


dzen
  ∷ IORef.IORef (Maybe (SysProc.ProcessHandle, Handle, Async.Async ()))
  -- ^ Process handle, stdin of the process, and timeout timer thread handle
  → String
  → String
  → IO ()
dzen procRef text fgColor = do
  (procHandler, input, threadHandle) ←
    IORef.readIORef procRef >>= \case
      Nothing → getNewProc
      Just (procHandler, input, threadHandle) →
        SysProc.getProcessExitCode procHandler >>= \case
          Just _ → Async.cancel threadHandle >> getNewProc
          Nothing → do
            Async.cancel threadHandle
            (procHandler, input,) <$> runKiller input procHandler

  fireAndForget $ hPutStrLn input colorfulText >> hFlush input
  IORef.writeIORef procRef $ Just (procHandler, input, threadHandle)

  where
    wmTitle = "wenzels-i3-status-generator"
    timeoutSeconds = 1 ∷ Word
    (w, h, x, y) = (120, 120, -100, 100); w, h ∷ Word; x, y ∷ Int
    (bgColor, fgDefaultColor) = ("black", "white")
    (fontFamily, fontStyle) = ("Hack", "bold")

    fontSize ∷ Word
    fontSize
      | length text ≤ 2 = 70
      | length text ≡ 3 = 42
      | length text ≡ 4 = 32
      | otherwise = 9

    fontStr (size ∷ Word)
      = "-*-"
      ⋄ fontFamily ⋄ "-"
      ⋄ fontStyle  ⋄ "-*-*-*-"
      ⋄ show size  ⋄ "-*-*-*-*-*-*-*"

    colorfulText = "^fn(" ⋄ fontStr fontSize ⋄ ")^fg(" ⋄ fgColor ⋄ ")" ⋄ text

    args =
      [ "-ta", "c"
      , "-title-name", wmTitle
      -- , "-p", show timeoutSeconds
      , "-w", show w, "-h", show h
      , "-x", show (if x < 0 then x − fromIntegral w else x)
      , "-y", show (if y < 0 then y − fromIntegral h else y)
      , "-bg", bgColor, "-fg", fgDefaultColor
      , "-fn", fontStr 9
      ]

    runKiller input procHandler = Async.async $ do
      threadDelay $ fromIntegral timeoutSeconds × 1000 × 1000
      hClose input >> SysProc.terminateProcess procHandler

    getNewProc = do
      (Just input, Nothing, Nothing, procHandler)
        ← SysProc.createProcess (SysProc.proc "dzen2" args)
        { SysProc.std_in  = SysProc.CreatePipe
        , SysProc.std_out = SysProc.NoStream
        , SysProc.std_err = SysProc.NoStream
        }

      addFinalizer procHandler $ SysProc.terminateProcess procHandler
      (procHandler, input,) <$> runKiller input procHandler
