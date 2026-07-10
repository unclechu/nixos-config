# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# Generic xinput pointer setup script.

from std/locks import Lock, withLock, initLock, deinitLock
from std/syncio import nil

# To make sure there are no stderr clashes when multiple processes/threads are writing to it
var stderrLock: Lock

# Write to stderr clash-free
template writeStderr*(line: string): void =
  withLock stderrLock:
    syncio.writeLine(syncio.stderr, line)
    syncio.flushFile(syncio.stderr)

template withStderr*(body: untyped): untyped =
  (initLock(stderrLock); try: body finally: deinitLock(stderrLock))

type CustomStderrWriter* = object
template writeLogLine*(writer: CustomStderrWriter, line: string): void = writeStderr(line)
