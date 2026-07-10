# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

from std/os import raiseOSError, osLastError, OSErrorCode
from std/posix import Sigset, sigemptyset, sigaddset, pthread_sigmask

from log as logger import Log, debug
from either import Either, right, left

const terminationSignals*: array[7, cint] = [
  posix.SIGINT, posix.SIGTERM, posix.SIGHUP, posix.SIGPIPE,
  posix.SIGTRAP, posix.SIGABRT, posix.SIGQUIT,
]

const knownSignals*: array[7, string] =
  ["INT", "TERM", "HUP", "PIPE", "TRAP", "ABRT", "QUIT"]

var terminationSigset: posix.Sigset

# Make the app block termination signals, for graceful termination
proc blockTerminationSignals*(): void =
  # Initialize the `Sigset` as an empty set
  if sigemptyset(terminationSigset) != 0: raiseOSError(osLastError())
  # Add all the termination signals to the `Sigset`
  for signal in terminationSignals:
    if sigaddset(terminationSigset, signal) != 0: raiseOSError(osLastError())
  # Also listen to SIGUSR1
  if sigaddset(terminationSigset, posix.SIGUSR1) != 0: raiseOSError(osLastError())
  # I don’t need to restore the old mask, but `pthread_sigmask` required is
  var ignoredOldMask: Sigset
  # `pthread_sigmask` returns the error number directly
  let error: cint = pthread_sigmask(posix.SIG_BLOCK, terminationSigset, ignoredOldMask)
  if error != 0: raiseOSError(OSErrorCode(error))

# Blocks until termination signal is received
proc waitForTerminationSignal*[TimeFormat: static string, Writer](
  log: Log[TimeFormat, Writer],
): Either[string, cint] {.gcsafe.} =
  log.debug("waitForTerminationSignal: Waiting for termination signal")
  var signal: cint
  let error = posix.sigwait(terminationSigset, signal)
  if error == 0:
    log.debug("waitForTerminationSignal: Received signal: " & $signal)
    right[string, cint](signal)
  else:
    log.error("waitForTerminationSignal: sigwait failed with code: " & $error)
    left[string, cint]("sigwait failed with error code: " & $error)
