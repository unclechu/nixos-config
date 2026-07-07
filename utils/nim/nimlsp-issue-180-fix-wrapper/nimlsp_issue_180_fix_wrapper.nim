# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# A wrapper for `nimlsp` that fixes https://github.com/PMunch/nimlsp/issues/180

from std/os import commandLineParams, osLastError, raiseOSError
from std/posix import Pid, SIGHUP, getpid, kill, execvp

const nimlspExecutable {.strdefine.}: string = "nimlsp"

proc getppid(): Pid {.importc, header: "<unistd.h>".}
proc prctl(option: cint): cint {.importc, header: "<sys/prctl.h>", varargs.}
let prSetPdeathsig {.importc: "PR_SET_PDEATHSIG", header: "<sys/prctl.h>".}: cint

proc dieWithParent*(signal: cint = SIGHUP): void =
  let originalParent = getppid()
  if prctl(prSetPdeathsig, signal.culong, 0.culong, 0.culong, 0.culong) != 0:
    raiseOSError(osLastError())
  # The parent may have died between getppid() and prctl().
  if getppid() != originalParent: discard kill(getpid(), signal)

proc main(): void =
  dieWithParent()
  let args = @[nimlspExecutable] & commandLineParams()
  let argv = allocCStringArray(args)
  defer: deallocCStringArray(argv)
  discard execvp(nimlspExecutable, argv)
  # Only runs after failure (execvp returns only on failure)
  let error = osLastError()
  deallocCStringArray(argv)
  raiseOSError(error)

main()
