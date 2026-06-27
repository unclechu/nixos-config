-- Author: Viacheslav Lotsmanov
-- License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnicodeSyntax #-}

module WenzelsI3StatusGenerator.ParentProc (dieWithParent) where

import "base" Control.Monad (void)

import qualified "base" Foreign.C.Types as CTypes

#include <signal.h>
#include <linux/prctl.h>


foreign import ccall "sys/prctl.h prctl"
  prctl
    ∷ CTypes.CInt
    → CTypes.CULong
    → CTypes.CULong
    → CTypes.CULong
    → CTypes.CULong
    → IO CTypes.CInt


dieWithParent ∷ IO ()
dieWithParent = void $ prctl (#const PR_SET_PDEATHSIG) (#const SIGHUP) 0 0 0
