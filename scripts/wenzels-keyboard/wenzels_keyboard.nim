from std/envvars import nil
from std/locks import nil
from std/monotimes import nil
from std/options import nil
from std/os import nil
from std/osproc import nil
from std/parseopt import nil
from std/posix import nil
from std/sequtils import nil
from std/streams import nil
from std/strutils import nil
from std/times import nil

type
  # Subprocess command
  Command {.requiresInit.} = object
    cmd: string
    args: seq[string] = @[]

  # Subprocess in-out communication handle
  InOutProc {.requiresInit.} = object
    process: osproc.Process
    # Call this `close` for proper resources cleanup instead of `process.close`.
    # But close only after process is terminated to avoid blocking.
    close: proc () {.raises: [IOError, OSError], gcsafe.}
    stdinStream: streams.Stream
    stdoutStream: streams.Stream

  # Parsed command-line arguments
  CommandLineArgs {.requiresInit.} = object
    noXlibHack: bool
    keyRepeatedDelay: uint
    keyRepeatedInterval: uint
    xkbLayout: string
    xkbOptions: string

let uid: posix.Uid = posix.geteuid()
let pid: posix.Pid = posix.getpid()

# These signals will be blocked from direct interaction with the application.
# The app will listen to them and will gracefully shutdown when receiving any of them.
const terminationSignals: array[7, cint] = [
  posix.SIGINT, posix.SIGTERM, posix.SIGHUP, posix.SIGPIPE,
  posix.SIGTRAP, posix.SIGABRT, posix.SIGQUIT,
]

const knownSignals: array[7, string] = ["INT", "TERM", "HUP", "PIPE", "TRAP", "ABRT", "QUIT"]

# Standard error handling

# To make sure there are no stderr clashes when multiple processes/threads are writing to it
var stderrLock: locks.Lock

# Write to stderr clash-free
proc writeStderr(line: string): void {.inline.} =
  locks.withLock stderrLock: (stderr.writeLine(line); stderr.flushFile())

template withStderr(body: untyped) =
  (locks.initLock(stderrLock); try: body finally: locks.deinitLock(stderrLock))

# Fail the program with a message written to stderr
proc fail(msg: string, exitCode: int = 1): void {.inline.} =
  writeStderr("[FAIL] " & msg); quit exitCode

# Logging

type Log = object

proc log(_: typedesc[Log], msg: string): void {.inline.} =
  writeStderr("[" & times.format(times.now(), "HH:mm:ss'.'fff") & "]" & msg)

proc debug(_: typedesc[Log], msg: string): void {.inline.} = Log.log("[DEBUG] " & msg)
proc warn(_: typedesc[Log], msg: string): void {.inline.} = Log.log("[WARNING] " & msg)
proc stage(_: typedesc[Log], msg: string): void {.inline.} = Log.log("[STAGE] " & msg)
proc skip(_: typedesc[Log], msg: string): void {.inline.} = Log.log("[SKIP] " & msg)
proc ok(_: typedesc[Log], msg: string): void {.inline.} = Log.log("[OK] " & msg)
proc error(_: typedesc[Log], msg: string): void {.inline.} = Log.log("[ERROR] " & msg)

# Executable name constants

const env: string = "env"
const setsid: string = "setsid"
const pgrep: string = "pgrep"
const xset: string = "xset"
const setxkbmap: string = "setxkbmap"
const numlockx: string = "numlockx"
const notifySend: string = "notify-send"
const xbindkeys: string = "xbindkeys"
const wenzelsXlibKeysHack: string = "wenzels-xlib-keys-hack"

# Not adding to `knownExecutables` by design
# (Nix should not add them as dependencies directly).
const wenzelsKeyboard: string = "wenzels-keyboard"
const xlibKeysHack: string = "xlib-keys-hack"
const xlibKeysHackWatchForWindowFocusEvents: string = "xlib-keys-hack-watch-for-window-focus-events"

const knownExecutables: array[9, string] = [
  env,
  setsid,
  pgrep,
  xset,
  setxkbmap,
  numlockx,
  notifySend,
  xbindkeys,
  wenzelsXlibKeysHack,
]

var uncheckedExecutables: seq[string] = @knownExecutables

const artifactExecutables: array[4, string] = [
  xbindkeys,
  wenzelsXlibKeysHack,
  xlibKeysHack,
  xlibKeysHackWatchForWindowFocusEvents,
]

template assertExecutableNames(executableNames: untyped): void =
  for name in executableNames: doAssert strutils.allCharsInSet(name, {'a'..'z', '-'})
# Assert executable name restrictions in compile-time
# (so that they can be safely interpolated into `pgrep` regexes).
static:
  assertExecutableNames(knownExecutables)
  assertExecutableNames(artifactExecutables)

# Mark as {.used.} because runtime dependencies checking is removed for Nix derivation
proc needExe(executableName: string): void {.used.} =
  if not (executableName in knownExecutables):
    fail(
      "Unknown executable: " & strutils.escape(executableName) &
      " (must be one of: " & $knownExecutables & ")"
    )
  elif os.findExe(executableName) == "":
    fail("Missing executable dependency: " & strutils.escape(executableName))
  else:
    sequtils.keepItIf(uncheckedExecutables, it != executableName)

# Mark as {.used.} because runtime dependencies checking is removed for Nix derivation
proc allKnownExecutablesAreChecked(): void {.used,inline.} =
  if uncheckedExecutables.len > 0:
    fail("Some executables left unchecked: " & $uncheckedExecutables)

# Redirect stderr of a subprocess to the main process stderr, line-by-line, clash-free.
proc forwardStderr(p: osproc.Process): void {.thread.} =
  let source = osproc.errorStream(p)
  var line: string
  while streams.readLine(source, line): writeStderr(line)

# Polling helpers

const pollTimeout: times.Duration = times.initDuration(seconds = 5)
const pollIntervalMs: uint = 100

type Poll = object

proc getPollDeadline(_: typedesc[Poll]): monotimes.MonoTime {.inline.} =
  monotimes.`+`(monotimes.getMonoTime(), pollTimeout)

# Wait for the condition expression to result to `true` and result into `true`
# or to `false` if `pollTimeout` is hit.
template waitForCondition(_: typedesc[Poll], condition: bool): bool =
  block:
    let deadline: monotimes.MonoTime = Poll.getPollDeadline()
    var satisfied: bool = true
    while not (condition):
      if monotimes.`<=`(deadline, monotimes.getMonoTime()): (satisfied = false; break)
      os.sleep(pollIntervalMs.int)
    satisfied

# Subprocess helpers

type SubProc = object

# Patch a `Command` so that
proc withResetSignals(
  _: typedesc[SubProc],
  cmd: Command,
  signals: seq[string],
): Command {.inline.} =
  for signal in signals:
    if not (signal in knownSignals):
      raiseAssert("Signal " & strutils.escape(signal) & " is not one of " & $knownSignals)
  Command(
    cmd: setsid,
    args: @["--", env, "--default-signal=" & strutils.join(signals, ","), "--", cmd.cmd] & cmd.args
  )

# Open a new process with stdin and stdout streams
proc startInOutInteraction(
  _: typedesc[SubProc],
  cmd: Command,
  resetSignals: seq[string] = @[],
): InOutProc =
  let newCmd = if resetSignals.len > 0: SubProc.withResetSignals(cmd, resetSignals) else: cmd
  Log.debug("startInOutInteraction: " & $newCmd)
  let p = osproc.startProcess(newCmd.cmd, args = newCmd.args, options = {osproc.poUsePath})
  var stderrThread: Thread[osproc.Process]
  createThread(stderrThread, forwardStderr, p)
  let close = proc (){.raises: [IOError, OSError], gcsafe.} =
    joinThread(stderrThread)
    osproc.close(p)
  InOutProc(
    process: p,
    close: close,
    stdinStream: osproc.inputStream(p),
    stdoutStream: osproc.outputStream(p),
  )

# Spawn a sub-process in background inheriting stdin, stdout, and stderr.
proc spawn(_: typedesc[SubProc], cmd: Command, resetSignals: seq[string] = @[]): void {.inline.} =
  let newCmd = if resetSignals.len > 0: SubProc.withResetSignals(cmd, resetSignals) else: cmd
  Log.debug("spawn: " & $newCmd)
  discard osproc.startProcess(
    newCmd.cmd,
    args = newCmd.args,
    options = {osproc.poUsePath, osproc.poDaemon, osproc.poParentStreams}
  )

# Start a sub-process in background inheriting stdin, stdout, and stderr and wait for success.
proc callCmd(_: typedesc[SubProc], cmd: Command, resetSignals: seq[string] = @[]): void {.inline.} =
  let newCmd = if resetSignals.len > 0: SubProc.withResetSignals(cmd, resetSignals) else: cmd
  Log.debug("callCmd: " & $newCmd)
  let p: osproc.Process = osproc.startProcess(
    newCmd.cmd,
    args = newCmd.args,
    options = {osproc.poUsePath, osproc.poParentStreams}
  )
  doAssert (osproc.waitForExit(p) == 0)
  osproc.close(p)

# Start a sub-process and just return the handle.
proc startCmd(
  _: typedesc[SubProc],
  cmd: Command,
  resetSignals: seq[string] = @[],
): osproc.Process {.inline.} =
  let newCmd = if resetSignals.len > 0: SubProc.withResetSignals(cmd, resetSignals) else: cmd
  Log.debug("startCmd: " & $newCmd)
  osproc.startProcess(
    newCmd.cmd,
    args = newCmd.args,
    options = {osproc.poUsePath, osproc.poParentStreams}
  )

# Desktop notifications

type DesktopNotification = object

proc notify(
  _: typedesc[DesktopNotification],
  title: string,
  text: string = "",
  urgent: bool = false,
): void {.inline.} =
  var args: seq[string] = @[]
  if urgent: args.add(@["-u", "critical"])
  args.add(@["--", title])
  if text != "": args.add(text)
  SubProc.spawn(Command(cmd: notifySend, args: args))

template withLockFile(path: string; body: untyped) =
  block:
    let
      currentTime: times.Time = times.getTime()
      lockPath: string = path
      lockToken: string = $pid & ":" & $times.toUnix(currentTime) & "\n"

    if os.fileExists(lockPath):
      let ageSeconds: int64 =
        times.inSeconds(times.`-`(currentTime, os.getLastModificationTime(lockPath)))
      if ageSeconds <= 60:
        DesktopNotification.notify(
          wenzelsKeyboard & "locked",
          "Lock file already exists: " & lockPath,
          urgent = true,
        )
        fail("Lock file already exists: " & lockPath)
      Log.debug("Overriding stale lock file: " & lockPath)

    Log.debug("Acquiring lock file: " & lockPath)
    writeFile(lockPath, lockToken)

    try:
      body
    finally:
      try:
        Log.debug("Releasing lock file: " & lockPath)
        # Avoid deleting a lock that another process may have replaced.
        if os.fileExists(lockPath) and readFile(lockPath) == lockToken:
          os.removeFile(lockPath)
        else:
          Log.warn("Another process took over the lock file: " & lockPath)
      except IOError, OSError:
        discard

# Working with POSIX PIDs

type Pid = object

proc findExecutablePids(_: typedesc[Pid], executableName: string): seq[uint] =
  # Make sure pgrep regex substitution is safe
  doAssert strutils.allCharsInSet(executableName, {'a'..'z', '-'})
  let p: InOutProc = SubProc.startInOutInteraction(Command(
    cmd: pgrep,
    args: @[
      "-u", $uid,
      "-f", "^(/[^ ]+ )?(|/[^ ]+/)?(" & executableName & "|[.]" & executableName & "-wrapped)($| )",
    ]
  ))
  streams.close(p.stdinStream)
  var line: string
  var pids: seq[uint] = @[]
  while streams.readLine(p.stdoutStream, line): pids.add(strutils.parseUInt(line))
  let exitCode: int = osproc.waitForExit(p.process)
  if not ((exitCode == 0 and pids.len > 0) or (exitCode == 1 and pids.len == 0)):
    fail(
      "findExecutablePids: Unexpected pgrep outcome " &
      "(exit code: " & $exitCode & "; pids.len: " & $pids.len & ")"
    )
  p.close()
  pids

proc terminate(_: typedesc[Pid], pid: uint): void {.raises: [OSError], inline.} =
  doAssert (pid != 0)
  if posix.kill(posix.Pid(pid), posix.SIGTERM) == 0: return
  let error: cint = posix.errno
  case posix.errno
  of posix.ESRCH: return # The process is already dead, considering it a success
  else: os.raiseOSError(os.OSErrorCode(error))

proc forceKill(_: typedesc[Pid], pid: uint): void {.raises: [OSError], inline.} =
  doAssert (pid != 0)
  if posix.kill(posix.Pid(pid), posix.SIGKILL) == 0: return
  let error: cint = posix.errno
  case posix.errno
  of posix.ESRCH: return # The process is already dead, considering it a success
  else: os.raiseOSError(os.OSErrorCode(error))

proc isProcessAlive(_: typedesc[Pid], pid: uint): bool {.raises: [OSError], inline.} =
  doAssert (pid != 0)
  if posix.kill(posix.Pid(pid), cint(0)) == 0: return true
  let error: cint = posix.errno
  case posix.errno
  of posix.EPERM: return true # The process exists, but we lack permission to signal it
  of posix.ESRCH: return false # No process currently has this PID (not alive)
  else: os.raiseOSError(os.OSErrorCode(error))

# Try to terminate a list of PIDs, or kill them if they refuse to terminate.
proc terminateOrKill(_: typedesc[Pid], pids: seq[uint]): void {.gcsafe.} =
  Log.debug("terminateOrKill: Terminating these PIDs: " & $pids)
  for pid in pids: Pid.terminate(pid)
  template allDead(): bool =
    block:
      var isAllDead: bool = true
      for pid in pids: (if Pid.isProcessAlive(pid): (isAllDead = false; break))
      isAllDead
  if Poll.waitForCondition(allDead()):
    Log.debug("terminateOrKill: These PIDs are successfully terminated: " & $pids)
    return
  Log.warn("terminateOrKill: Some of these PIDs are still alive (killing them now): " & $pids)
  for pid in pids: Pid.forceKill(pid)
  if Poll.waitForCondition(allDead()):
    Log.warn("terminateOrKill: These PIDs were successfully killed: " & $pids)
    return
  let errMsg: string = "terminateOrKill: Failed to kill PIDs: " & $pids
  DesktopNotification.notify(wenzelsKeyboard, errMsg, urgent = true)
  fail(errMsg)

# Parsing command-line-arguments

proc parseCommandLineArgs(): CommandLineArgs =
  var noXlibHack: bool = false
  var keyRepeatedDelay: options.Option[uint] = options.none(uint)
  var keyRepeatedInterval: options.Option[uint] = options.none(uint)
  var xkbLayout: options.Option[string] = options.none(string)
  var xkbOptions: options.Option[string] = options.none(string)
  let usageInfo: string = strutils.join([
    "Usage: " & os.extractFilename(os.getAppFilename()) & " REQUIRED [OPTIONAL]",
    "",
    "REQUIRED options:",
    "  --key-repeated-delay     UINT",
    "  --key-repeated-interval  UINT",
    "  --xkb-layout             STR",
    "  --xkb-options            STR",
    "",
    "OPTIONAL options:",
    "  -h --help                Show this usage info",
    "  --no-xlib-hack           Do not start xlib-keys-hack",
  ], "\n")
  for kind, key, value in parseopt.getopt(
    shortNoVal = {'h'},
    longNoVal = @[
      "help",
      "no-xlib-hack",
      "key-repeated-delay",
      "key-repeated-interval",
      "xkb-layout",
      "xkb-options",
    ],
  ):
    case kind
    of parseopt.cmdEnd: discard
    of parseopt.cmdArgument: fail("This application does not accept any positional arguments", 2)
    of parseopt.cmdShortOption, parseopt.cmdLongOption:
      case key
      of "h", "help": (stdout.writeLine(usageInfo); quit 0)
      of "no-xlib-hack": noXlibHack = true
      of "key-repeated-delay": keyRepeatedDelay = options.some(strutils.parseUInt(value))
      of "key-repeated-interval": keyRepeatedInterval = options.some(strutils.parseUInt(value))
      of "xkb-layout": (doAssert (value != ""); xkbLayout = options.some(value))
      of "xkb-options": (doAssert (value != ""); xkbOptions = options.some(value))
      else: fail("Unknown option: " & strutils.escape(key) & "\n\n" & usageInfo, 2)
  template requiredFail(arg: string): void =
    fail("Required argument " & strutils.escape("--" & arg) & " is not provided\n\n" & usageInfo, 2)
  if options.isNone keyRepeatedDelay: requiredFail("key-repeated-delay")
  if options.isNone keyRepeatedInterval: requiredFail("key-repeated-interval")
  if options.isNone xkbLayout: requiredFail("xkb-layout")
  if options.isNone xkbOptions: requiredFail("xkb-options")
  CommandLineArgs(
    noXlibHack: noXlibHack,
    keyRepeatedDelay: options.get(keyRepeatedDelay),
    keyRepeatedInterval: options.get(keyRepeatedInterval),
    xkbLayout: options.get(xkbLayout),
    xkbOptions: options.get(xkbOptions),
  )

# Sub-processes supervision

# Clean all the processes that are potentially still running
proc cleanArtifacts(): void =
  var artifactPids: seq[uint] = @[]
  for x in artifactExecutables:
    let pids: seq[uint] = Pid.findExecutablePids(x)
    Log.debug("cleanArtifacts: " & strutils.escape(x) & " PIDs: " & $pids)
    artifactPids.add(pids)
  Log.debug("cleanArtifacts: Terminating PIDs: " & $artifactPids)
  Pid.terminateOrKill(artifactPids)

# Take over previous run of `wenzels-keyboard.lock`
proc takeOverPreviousRun(): void =
  const cmd: string = strutils.escape(wenzelsKeyboard)
  Log.stage("Taking over of the previous run of " & cmd)

  let pids: seq[uint] = sequtils.filterIt(Pid.findExecutablePids(wenzelsKeyboard), it != pid.uint)
  if pids.len == 0: Log.skip("No previous run of " & cmd & " found")
  else: Pid.terminateOrKill(pids)

  Log.stage("Cleaning potentially survived artifacts: " & $artifactExecutables)
  cleanArtifacts()

  Log.ok("Took over of the previous " & cmd & " run (" & $pids & ")")

# Main events bus

type
  MainEventKind = enum
    signalReceived
    signalWaitFailed
    workersFinished

  MainEvent = object
    case kind: MainEventKind
    of signalReceived: signal: cint
    of signalWaitFailed: errorCode: cint
    of workersFinished: discard

var mainEvents: Channel[MainEvent]

# Sub processes workers (supervising indefinitely running daemons)

type
  SubProcWorkerEventKind = enum
    started # Worker’s sub-process is started
    exited # Worker’s sub-process has exited (see `exitCode`)
    failed # Worker has failed unexpectedly
    cleanup # An outside world signal to terminate all the workers

  SubProcWorkerEvent {.requiresInit.} = object
    case kind: SubProcWorkerEventKind
    of started:
      startedCmd: ref Command
      pid: uint
    of exited:
      exitedCmd: ref Command
      exitCode: int
    of failed:
      failedCmd: ref Command
      message: string
    of cleanup: discard

  SubProcWorkerSpec {.requiresInit.} = object
    thread: Thread[ref Command]
    subProcPid: options.Option[uint]
    isRunning: bool

var subProcWorkerEvents: Channel[ref SubProcWorkerEvent]

proc startSubProcWorker(cmd: ref Command): void {.thread gcsafe.} =
  var p: osproc.Process = nil
  try:
    p = SubProc.startCmd(Command(cmd: cmd.cmd, args: cmd.args), resetSignals = @["TERM"])
    let pid: uint = osproc.processID(p).uint
    subProcWorkerEvents.send((ref SubProcWorkerEvent)(kind: started, startedCmd: cmd, pid: pid))
    let exitCode: int = osproc.waitForExit(p)
    subProcWorkerEvents.send(
      (ref SubProcWorkerEvent)(kind: exited, exitedCmd: cmd, exitCode: exitCode)
    )
  # Prevent exceptions from propagating to the main thread causing the app to ungracefully fail
  except CatchableError as error:
    subProcWorkerEvents.send(
      (ref SubProcWorkerEvent)(kind: failed, failedCmd: cmd, message: error.msg)
    )
  finally:
    # Cleanup the process if it was started successfully
    if p != nil: (try: osproc.close(p) except CatchableError: discard)

proc startSubProcWorkers(args: CommandLineArgs): void {.thread.} =
  try:
    Log.debug("startSubProcWorkers: Thread starting")

    var threads: seq[ref SubProcWorkerSpec] = @[]
    let noPidYet: options.Option[uint] = options.none(uint)

    block addWorker:
      let cmd: ref Command = (ref Command)(cmd: xbindkeys, args: @["--nodaemon"])
      var spec: ref SubProcWorkerSpec =
        (ref SubProcWorkerSpec)(
          thread: default(Thread[ref Command]),
          subProcPid: noPidYet,
          isRunning: true
        )
      Log.debug("startSubProcWorkers: Spawning worker thread for: " & $cmd[])
      createThread(spec.thread, startSubProcWorker, cmd)
      threads.add(spec)

    block addWorker:
      let cmd: ref Command = (ref Command)(cmd: wenzelsXlibKeysHack)
      var spec: ref SubProcWorkerSpec =
        (ref SubProcWorkerSpec)(
          thread: default(Thread[ref Command]),
          subProcPid: noPidYet,
          isRunning: true
        )
      if args.noXlibHack:
        Log.debug("startSubProcWorkers: NOT spawning (--no-xlib-hack) worker thread for: " & $cmd[])
        break addWorker
      else:
        Log.debug("startSubProcWorkers: Spawning worker thread for: " & $cmd[])
      createThread(spec.thread, startSubProcWorker, cmd)
      threads.add(spec)

    template getEventCmd(event: ref SubProcWorkerEvent): ref Command =
      case event.kind
      of started: event.startedCmd
      of exited: event.exitedCmd
      of failed: event.failedCmd
      of cleanup: raiseAssert "You are not supposed to use getEventCmd for event.kind = cleanup"

    template updateThread(event: ref SubProcWorkerEvent, thread: untyped, body: untyped): void =
      block:
        let cmd: Command = getEventCmd(event)[]
        var found: bool = false
        for thread in threads: (if thread.thread.data[] == cmd: (body; found = true; break))
        if not found: fail("startSubProcWorkers: Unexpected worker event cmd: " & $cmd)

    template terminateAll(): void =
      block:
        Log.debug("startSubProcWorkers: Terminating all the sub processes that are still running")
        var pids: seq[uint] = @[]
        for thread in threads:
          let subProcPid: options.Option[uint] = thread.subProcPid
          if options.isNone(subProcPid):
            Log.warn(
              "startSubProcWorkers/terminateAll: No PID reported yet for " & $thread.thread.data[]
            )
          else:
            pids.add(options.get(subProcPid))
        Pid.terminateOrKill(pids)

    template notifyWhenReady(cmd: string): void =
      block ready:
        DesktopNotification.notify(
          wenzelsKeyboard,
          strutils.escape(cmd) & " worker is started",
        )
        for thread in threads:
          if (not thread.isRunning) or options.isNone(thread.subProcPid):
            break ready
        DesktopNotification.notify(
          wenzelsKeyboard,
          "All " & $threads.len & " worker(s) started",
        )

    block eventLoop:
      while true:
        # If no threads are running anymore we are done here
        block areAllDone:
          for thread in threads: (if thread.isRunning: break areAllDone)
          Log.debug("startSubProcWorkers: No threads are still running, wrapping things up")
          # Wait for all the threads to be done
          if Poll.waitForCondition(
            block:
              var allThreadsAreDone: bool = true
              for thread in threads: (if running(thread.thread): (allThreadsAreDone = false; break))
              allThreadsAreDone
          ):
            Log.error(
              "startSubProcWorkers: Some of the threads seems to be stuck (not joining them)"
            )
          else:
            Log.debug("startSubProcWorkers: All threads are done, joining them")
            for thread in threads: joinThread(thread.thread)
          Log.debug("startSubProcWorkers: The event loop is done")
          break eventLoop
        let event: ref SubProcWorkerEvent = subProcWorkerEvents.recv()
        case event.kind:
        of started:
          Log.debug(
            "startSubProcWorkers: Thread reports PID " & $event.pid &
            " for " & $getEventCmd(event)[]
          )
          updateThread(event, thread): thread.subProcPid = options.some(event.pid)
          notifyWhenReady(getEventCmd(event)[].cmd)
        of exited:
          Log.debug(
            "startSubProcWorkers: Thread reports exit code " & $event.exitCode &
            " for " & $getEventCmd(event)[]
          )
          updateThread(event, thread): thread.isRunning = false
          # If any other thread is still running terminate it
          block termination:
            for thread in threads: (if thread.isRunning: (terminateAll(); break termination))
        of failed:
          Log.error(
            "startSubProcWorkers: Thread reports failure for " &
            $getEventCmd(event)[] & ": " & strutils.escape(event.message)
          )
          updateThread(event, thread): thread.isRunning = false
          block termination:
            for thread in threads: (if thread.isRunning: (terminateAll(); break termination))
        of cleanup:
          Log.debug("startSubProcWorkers: Received cleanup request")
          block termination:
            for thread in threads: (if thread.isRunning: (terminateAll(); break termination))
  finally:
    Log.debug("startSubProcWorkers: Reporting subprocesses workers thread finished")
    mainEvents.send(MainEvent(kind: workersFinished))

# POSIX signals control

var terminationSigset: posix.Sigset

# Make the app block termination signals, for graceful termination.
proc blockTerminationSignals(): void =
  # Initialize the `Sigset` as an empty set
  if posix.sigemptyset(terminationSigset) != 0: os.raiseOSError(os.osLastError())
  # Add all the termination signals to the `Sigset`
  for signal in terminationSignals:
    if posix.sigaddset(terminationSigset, signal) != 0:
      os.raiseOSError(os.osLastError())
  # Also listen to SIGUSR1 so that `terminationSignalWaiter` thread can be asked to finish
  if posix.sigaddset(terminationSigset, posix.SIGUSR1) != 0:
    os.raiseOSError(os.osLastError())
  # I don’t need to restore the old mask, but `pthread_sigmask` required is
  var ignoredOldMask: posix.Sigset
  # `pthread_sigmask` returns the error number directly
  let error: cint = posix.pthread_sigmask(posix.SIG_BLOCK, terminationSigset, ignoredOldMask)
  if error != 0: os.raiseOSError(os.OSErrorCode(error))

# Shutdown

proc terminationSignalWaiter(): void {.thread.} =
  Log.debug("terminationSignalWaiter: Thread starting")
  var signal: cint
  let error = posix.sigwait(terminationSigset, signal)
  if error == 0:
    Log.debug("terminationSignalWaiter: Received signal: " & $signal)
    mainEvents.send(MainEvent(kind: signalReceived, signal: signal))
  else:
    Log.error("terminationSignalWaiter: sigwait failed with code: " & $error)
    mainEvents.send(MainEvent(kind: signalWaitFailed, errorCode: error))

# Main program, stages

# Basic keyboard behavior configurations
proc basicSetup(args: CommandLineArgs): void =
  for command in [
    Command(cmd: xset, args: @["r", "rate", $args.keyRepeatedDelay, $args.keyRepeatedInterval]),
    Command(cmd: setxkbmap, args: @["-layout", args.xkbLayout, "-option", args.xkbOptions]),
    Command(cmd: numlockx, args: @["off"]),
    Command(cmd: numlockx, args: @["on"]),
  ]: SubProc.callCmd(command)

withStderr:
  let xdgRuntimeDir: string = block:
    let envVarName: string = "XDG_RUNTIME_DIR"
    let x: string = envvars.getEnv(envVarName)
    if x == "": fail("Can’t read " & strutils.escape(envVarName))
    x

  # Guard dependencies (this piece is parsed by Nix, the shape is important to match the regex)
  needExe("env")
  needExe("setsid")
  needExe("pgrep")
  needExe("xset")
  needExe("setxkbmap")
  needExe("numlockx")
  needExe("notify-send")
  needExe("xbindkeys")
  needExe("wenzels-xlib-keys-hack")
  allKnownExecutablesAreChecked()

  let commandLineArgs: CommandLineArgs = parseCommandLineArgs()
  Log.debug("Parsed command-line arguments: " & $commandLineArgs)

  # Terminate previous run of wenzels-keyboard and its possible zombified artifacts
  # (or kill those if it they refuse to terminate within a timeout).
  takeOverPreviousRun()

  var
    signalThread: Thread[void]
    workersThread: Thread[CommandLineArgs]
    receivedSignal: options.Option[cint] = options.none(cint)

  try:
    withLockFile(xdgRuntimeDir & "/wenzels-keyboard.lock"):
      DesktopNotification.notify(wenzelsKeyboard, "Setting things up")

      Log.stage("Basic setup")
      basicSetup(commandLineArgs)

      Log.debug("Opening communications channels")
      subProcWorkerEvents.open()
      mainEvents.open()

      Log.debug("Blocking POSIX termination signals (for graceful shutdown)")
      blockTerminationSignals()

      Log.stage("Starting termination signal waiter thread")
      createThread(signalThread, terminationSignalWaiter)
      Log.stage("Starting subprocesses workers thread")
      createThread(workersThread, startSubProcWorkers, commandLineArgs)

      block mainEventLoop:
        let event = mainEvents.recv()
        case event.kind
        of signalReceived:
          Log.stage("Received termination signal (terminating): " & $event.signal)
          receivedSignal = options.some(event.signal)
          DesktopNotification.notify(
            wenzelsKeyboard & " exiting",
            "Received termination signal: " & $event.signal,
          )
          break mainEventLoop
        of workersFinished:
          Log.warn(
            "Subprocesses workers thread reported finishing " &
            "(probably watched subprocess died, terminating)"
          )
          DesktopNotification.notify(
            wenzelsKeyboard & " exiting",
            "Subprocess worker thread exited early",
            urgent = true,
          )
          break mainEventLoop
        of signalWaitFailed:
          DesktopNotification.notify(
            wenzelsKeyboard & " exiting",
            "Termination signal waiting failed",
            urgent = true,
          )
          fail("Signal waiter failed with error code: " & $event.errorCode)

  finally:
    Log.stage("Cleaning up")

    Log.debug("Sending clean up event to subprocesses workers thread")
    subProcWorkerEvents.send((ref SubProcWorkerEvent)(kind: cleanup))
    Log.debug("Waiting for the subprocesses workers thread to finish")
    if Poll.waitForCondition(not running(workersThread)):
      Log.debug("Joining the subprocesses workers thread")
      joinThread(workersThread)
    else:
      Log.error("Failed to cleanup subprocesses workers thread (ignoring it)")

    Log.debug("Triggering SIGUSR1 to make sure termination signal waiter thread is finishing")
    if posix.kill(pid, posix.SIGUSR1) != 0: os.raiseOSError(os.osLastError())
    Log.debug("Waiting for the termination signal waiter thread to finish")
    if Poll.waitForCondition(not running(signalThread)):
      Log.debug("Joining the termination signal waiter thread")
      joinThread(signalThread)
    else:
      Log.error("Failed to cleanup termination signal waiter thread (ignoring it)")

    Log.debug("Closing communication channels")
    subProcWorkerEvents.close()
    mainEvents.close()

    Log.debug("Cleaning up artifacts if any left (normally there should be none)")
    cleanArtifacts()

  Log.stage("Final exiting")
  if options.isNone(receivedSignal):
    Log.error("Unexpected exit, without a termination signal (probably watched process died)")
    quit 1
  else:
    # Conventional shell exit status: 128 + signal number.
    let finalSignal: int = 128 + options.get(receivedSignal)
    Log.ok("Received signal " & $options.get(receivedSignal) & ", exiting with " & $finalSignal)
    quit finalSignal
