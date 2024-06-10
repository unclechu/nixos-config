-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE UnicodeSyntax, ViewPatterns, LambdaCase #-}
{-# OPTIONS_GHC -Wall -Wno-missing-signatures -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant <$>" #-}

module Main (main) where

import Data.Maybe (listToMaybe)
import Data.List (find, sort, intercalate)
import Text.Read (readMaybe)
import Data.Char (toLower)

import Control.Monad (forM_, when)

import System.Directory (listDirectory)
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import System.IO (stderr, hPutStrLn)
import qualified System.Posix.IO as PosixIO

main ∷ IO ()
main = do
  switchToMode ←
    getArgs >>= \case
      [parseCpuMode → Just mode] → pure mode

      [x] | x == "-h" || x == "--help" → do
        putStrLn usageInfo
        exitSuccess

      args →
        fail $ unlines
          [ "Unrecognized arguments: " <> show args
          , mempty
          , usageInfo
          ]

  hPutStrLn stderr $ "Switching CPUs to " <> (show . serializeCpuMode) switchToMode <> " mode…"
  cpuModeControlFiles ← getCpuModeControlFiles
  cpuModes ← getCpuModes cpuModeControlFiles
  printModes cpuModes

  if all ((Just switchToMode ==) . parseCpuMode . snd) cpuModes
    then
      hPutStrLn stderr $
        "[OK] All CPUs are already in " <> (show . serializeCpuMode) switchToMode <> " mode"
    else do
      forM_ cpuModes $ \(fst → file) → do
        fd ← PosixIO.openFd file PosixIO.WriteOnly PosixIO.defaultFileFlags
        bytesCount ← PosixIO.fdWrite fd (serializeCpuMode switchToMode)

        when (bytesCount /= (fromIntegral . length . serializeCpuMode) switchToMode) $
          fail $ unwords
            [ "Failed to switch", file, "to", (show . serializeCpuMode) switchToMode, "mode,"
            , show bytesCount, "bytes written, expected", (show . length . serializeCpuMode) switchToMode
            ]

        PosixIO.closeFd fd

      hPutStrLn stderr $
        "[OK] All CPUs are switched to " <> (show . serializeCpuMode) switchToMode <> " mode"

      getCpuModes cpuModeControlFiles >>= printModes

usageInfo ∷ String
usageInfo = unlines
  [ "Usage example:"
  , "  cpu-mode-switch (" <> intercalate "|" [serializeCpuMode x | x ← [minBound .. maxBound]] <> ")"
  ]

data CpuMode = Powersave | Performance
  deriving (Eq, Show, Bounded, Enum)

parseCpuMode ∷ String → Maybe CpuMode
parseCpuMode x = find ((x ==) . fmap toLower . show) [minBound .. maxBound]

serializeCpuMode ∷ CpuMode → String
serializeCpuMode = fmap toLower . show

getCpuModeControlFiles ∷ IO [FilePath]
getCpuModeControlFiles = go where
  go = (fmap . fmap) ((dir <>) . (<> suffix)) getCpuDirs
  getCpuDirs = sort . filter isCpuDir <$> listDirectory dir

  (dir, _exampleCpuDir, suffix) =
    ("/sys/devices/system/cpu/", "cpu0", "/cpufreq/scaling_governor")

  isCpuDir ∷ String → Bool
  isCpuDir ('c':'p':'u': (readMaybe @Word → Just _)) = True
  isCpuDir _ = False

getCpuModes ∷ [FilePath] → IO [(FilePath, String)]
getCpuModes =
  mapM $ \file →
    (listToMaybe . lines <$> readFile file) >>= \case
      Nothing → fail $ "Failed to read CPU mode from " <> file <> " file"
      Just cpuMode → pure (file, cpuMode)

printModes ∷ [(FilePath, String)] → IO ()
printModes = mapM_ $ \(file, mode) → hPutStrLn stderr $ file <> " " <> mode
