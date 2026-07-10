# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

from std/posix import open, close, O_RDWR, O_CREAT, O_CLOEXEC
from std/oserrors import raiseOSError, osLastError
from std/strutils import escape
from std/options import Option, some, none

proc cFlock(fd: cint; operation: cint): cint
  {.importc: "flock", header: "<sys/file.h>".}

let
  LOCK_EX {.importc: "LOCK_EX", header: "<sys/file.h>".}: cint
  LOCK_UN {.importc: "LOCK_UN", header: "<sys/file.h>".}: cint
  LOCK_NB {.importc: "LOCK_NB", header: "<sys/file.h>".}: cint
  EWOULDBLOCK {.importc: "EWOULDBLOCK", header: "<errno.h>".}: cint
  EAGAIN {.importc: "EAGAIN", header: "<errno.h>".}: cint

# Only read-write for the owner
const lockFilePermissions: cint = 0o600

type FileLock* {.requiresInit.} = object
  fd: cint = -1

# Acquire a file lock.
#
# Returns `none` if couldn’t acquire lock (already acquired by other application).
proc acquireFileLock*(lockFilePath: string): Option[FileLock] {.inline.} =
  let fd = open(lockFilePath.cstring, O_RDWR or O_CREAT or O_CLOEXEC, lockFilePermissions)
  if fd < 0: raiseOSError(osLastError(), "open " & lockFilePath.escape)
  if cFlock(fd, LOCK_EX or LOCK_NB) == 0: return FileLock(fd: fd).some
  let err = osLastError()
  discard close(fd)
  if err.cint == EWOULDBLOCK or err.cint == EAGAIN: return FileLock.none
  raiseOSError(err, "flock " & lockFilePath.escape)

# Release a file lock.
proc releaseFileLock*(lock: var FileLock) {.inline.} =
  if lock.fd < 0: return
  discard cFlock(lock.fd, LOCK_UN)
  discard lock.fd.close
  lock.fd = -1
