-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | Module responsible of handling application state
module WenzelsI3StatusGenerator.Handler.AppState
     ( appStateHandler
     , module WenzelsI3StatusGenerator.Handler.AppState.Types
     ) where

import "base" Control.Exception (finally)
import "base" Control.Monad (when)
import qualified "async" Control.Concurrent.Async as Async

-- Local imports

import WenzelsI3StatusGenerator.Handler.AppState.Types
import WenzelsI3StatusGenerator.Layout (colorOfLayout)
import WenzelsI3StatusGenerator.Render (render)
import WenzelsI3StatusGenerator.Utils

import WenzelsI3StatusGenerator.Indicators
  ( showNumLock
  , colorOfNumLock
  , showCapsLock
  , colorOfCapsLock
  , showAlternativeState
  , colorOfAlternativeState
  )


-- | Reactive loop that gets state modifier from an @MVar@
--   to update the state and re-render it (if it's @Just@)
--   or terminate the application (it it's @Nothing@).
--
-- This function should be ran in its own thread and it should be the only
-- thread that writes to stdout.
--
-- This function is a store itself for an application "State".
-- It takes a function that reads next "State" modification but the "State"
-- itself is preserved within this function. Well, it also takes a function to
-- write the new "State" to just to let some other threads to read it for their
-- own purposes.
appStateHandler
  ∷ (Message → Color → IO ())
  -- ^ Report via Dzen (keyboard layout code or mode like “num” for num lock)
  → IO (State → State)
  -- ^ Read next application state modification function
  --   (blocks where there are no updates to handle;
  --   @Nothing@ we are done with the handling, application ends)
  → (State → IO ())
  -- ^ Write new application state (after receiving an update for it)
  → State
  → IO (Async.Async ())
appStateHandler reportCallback getNextStateUpdate writeState initialState = go where
  go = do
    echo "[" -- Open of a list stream
    echo $ render initialState -- Rendering initial state without comma separator
    Async.async $ loop initialState `finally` echo "]"

  loop s = getNextStateUpdate >>= handle s >>= loop
  darn = reportCallback "ERR" "#ff0000"

  -- | Handle one state modification
  handle ∷ State → (State → State) → IO State
  handle prevState stateModifier = f where
    f | newState ≡ prevState = pure prevState
      | otherwise = newState <$ newStateHandler

    newState = stateModifier prevState

    newStateHandler = do
      writeState newState
      echo $ "," ⋄ render newState
      report

    report = do
      when (kbdLayout newState ≢ kbdLayout prevState) $
        case kbdLayout newState of
          Just (Right layout) →
            reportCallback (show layout) (colorOfLayout layout)
          _ → darn

      do
        let
          condition
            = alternative newState ≢ alternative prevState
            ∧ ( fmap snd (alternative newState) ≡ Just True
              ∨ fmap snd (alternative prevState) ≡ Just True
              )
        when condition $
          either (const darn) (uncurry reportCallback) $ (,)
            <$> (showAlternativeState ∘ alternative) newState
            <*> (colorOfAlternativeState ∘ alternative) newState

      when (capsLock newState ≢ capsLock prevState) $
        reportCallback
          (showCapsLock ∘ capsLock $ newState)
          (colorOfCapsLock ∘ capsLock $ newState)

      when (numLock newState ≢ numLock prevState) $
        reportCallback
          (showNumLock ∘ numLock $ newState)
          (colorOfNumLock ∘ numLock $ newState)


-- * Types

type Message = String
type Color = String
