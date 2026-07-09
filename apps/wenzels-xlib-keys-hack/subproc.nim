# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# Sub-process running helpers.

from std/osproc import Process, startProcess, close, inputStream, outputStream, waitForExit
from std/streams import Stream, readLine
from std/strutils import escape, join

from log as logger import Log, debug
from needexe import nil
from signals import knownSignals

type
  # Subprocess command
  Command* {.requiresInit.} = object
    cmd*: string
    args*: seq[string] = @[]

# Patch a `Command` so that
proc withResetSignals(
  command: Command,
  signals: seq[string],
): Command =
  block:
    if signals.len <= 0:
      command # No signals to reset, returning original command
    else:
      for signal in signals:
        if not (signal in knownSignals):
          raiseAssert("Signal " & escape(signal) & " is not one of " & $knownSignals)
      Command(
        cmd: needexe.setsid,
        args: @[
          "--", needexe.env, "--default-signal=" & join(signals, ","),
          "--", command.cmd
        ] & command.args
      )

# Redirect a Stream stderr, line-by-line, clash-free.
proc redirectStreamToLog*[TimeFormat: static string, Writer](
  input: tuple[
    log: Log[TimeFormat, Writer],
    stream: streams.Stream,
    prefix: string,
  ],
): void {.thread.} =
  var line: string

  try:
    while readLine(input.stream, line):
      input.log.debug(if input.prefix.len > 0: input.prefix & ": " & line else: line)

  except IOError:
    # Normal during shutdown if the Process/pipe was closed while this
    # thread was blocked in readLine().
    input.log.debug("redirectStreamToLog: Stream closed for " & escape(input.prefix))

  except CatchableError as e:
    quit("redirectStreamToLog: Unexpected exception for " & escape(input.prefix) & ": " & e.msg)

# Start a sub-process and just return the handle.
template startCmd*[TimeFormat: static string, Writer](
  log: lent Log[TimeFormat, Writer],
  command: Command,
  resetSignals: seq[string] = @[],
  createStreams: bool = false,
): Process =
  block:
    let newCmd = withResetSignals(command, resetSignals)
    log.debug("startCmd: " & $newCmd)
    var options: set[osproc.ProcessOption] = {osproc.poUsePath}
    if not createStreams: options.incl(osproc.poParentStreams)
    startProcess(newCmd.cmd, args = newCmd.args, options = options)

# Start a sub-process in background inheriting stdin, stdout, and stderr and wait for success.
template callCmd*[TimeFormat: static string, Writer](
  logger: lent Log[TimeFormat, Writer],
  command: Command,
  resetSignals: seq[string] = @[],
): void =
  block:
    let newCmd = withResetSignals(command, resetSignals)
    logger.debug("callCmd: " & $newCmd)
    let p: Process = startProcess(newCmd.cmd, args = newCmd.args, options = {osproc.poUsePath})
    p.inputStream.close
    var stderrThread: Thread[tuple[log: Log[TimeFormat, Writer], stream: Stream, prefix: string]]
    var stdoutThread: Thread[tuple[log: Log[TimeFormat, Writer], stream: Stream, prefix: string]]
    let pfx: string = $newCmd & " "
    createThread(stderrThread, redirectStreamToLog, (logger, p.errorStream, pfx & "stderr"))
    createThread(stdoutThread, redirectStreamToLog, (logger, p.outputStream, pfx & "stdout"))
    doAssert (p.waitForExit == 0)
    p.close
    stderrThread.joinThread
    stdoutThread.joinThread

# Spawn a sub-process in background inheriting stdin, stdout, and stderr.
template spawn*[TimeFormat: static string, Writer](
  logger: lent Log[TimeFormat, Writer],
  command: Command,
  resetSignals: seq[string] = @[],
): void =
  block:
    let newCmd = withResetSignals(command, resetSignals)
    logger.debug("spawn: " & $newCmd)
    discard osproc.startProcess(
      newCmd.cmd,
      args = newCmd.args,
      options = {osproc.poUsePath, osproc.poDaemon, osproc.poParentStreams}
    )
