from std/envvars import nil
from std/locks import nil
from std/monotimes import nil
from std/options import nil
from std/os import nil
from std/osproc import nil
from std/posix import nil
from std/sequtils import nil
from std/streams import nil
from std/strutils import nil
from std/times import nil

from cliargs import nil
from log as logging import fail, debug, skip, ok, warn, stage, error
from needexe as exe import checkExecutableDependencies

checkExecutableDependencies()

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

type CustomStderrWriter = object
template writeLogLine(writer: CustomStderrWriter, line: string): void = writeStderr(line)
let log = logging.Log[logging.defaultTimeFormat, CustomStderrWriter]()

# Fail the program with a message
template fail(msg: string, exitCode: int = 1): void = (log.fail(msg); quit exitCode)

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
    cmd: exe.setsid,
    args: @["--", exe.env, "--default-signal=" & strutils.join(signals, ","), "--", cmd.cmd] & cmd.args
  )

# Open a new process with stdin and stdout streams
proc startInOutInteraction(
  _: typedesc[SubProc],
  cmd: Command,
  resetSignals: seq[string] = @[],
): InOutProc =
  let newCmd = if resetSignals.len > 0: SubProc.withResetSignals(cmd, resetSignals) else: cmd
  log.debug("startInOutInteraction: " & $newCmd)
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
  log.debug("spawn: " & $newCmd)
  discard osproc.startProcess(
    newCmd.cmd,
    args = newCmd.args,
    options = {osproc.poUsePath, osproc.poDaemon, osproc.poParentStreams}
  )

# Start a sub-process in background inheriting stdin, stdout, and stderr and wait for success.
proc callCmd(_: typedesc[SubProc], cmd: Command, resetSignals: seq[string] = @[]): void {.inline.} =
  let newCmd = if resetSignals.len > 0: SubProc.withResetSignals(cmd, resetSignals) else: cmd
  log.debug("callCmd: " & $newCmd)
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
  log.debug("startCmd: " & $newCmd)
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
  SubProc.spawn(Command(cmd: exe.notifySend, args: args))

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
          exe.wenzelsKeyboard & "locked",
          "Lock file already exists: " & lockPath,
          urgent = true,
        )
        fail("Lock file already exists: " & lockPath)
      log.debug("Overriding stale lock file: " & lockPath)

    log.debug("Acquiring lock file: " & lockPath)
    writeFile(lockPath, lockToken)

    try:
      body
    finally:
      try:
        log.debug("Releasing lock file: " & lockPath)
        # Avoid deleting a lock that another process may have replaced.
        if os.fileExists(lockPath) and readFile(lockPath) == lockToken:
          os.removeFile(lockPath)
        else:
          log.warn("Another process took over the lock file: " & lockPath)
      except IOError, OSError:
        discard

# Working with POSIX PIDs

type Pid = object

proc findExecutablePids(_: typedesc[Pid], executableName: string): seq[uint] =
  # Make sure pgrep regex substitution is safe
  doAssert strutils.allCharsInSet(executableName, {'a'..'z', '-'})
  let p: InOutProc = SubProc.startInOutInteraction(Command(
    cmd: exe.pgrep,
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
  log.debug("terminateOrKill: Terminating these PIDs: " & $pids)
  for pid in pids: Pid.terminate(pid)
  template allDead(): bool =
    block:
      var isAllDead: bool = true
      for pid in pids: (if Pid.isProcessAlive(pid): (isAllDead = false; break))
      isAllDead
  if Poll.waitForCondition(allDead()):
    log.debug("terminateOrKill: These PIDs are successfully terminated: " & $pids)
    return
  log.warn("terminateOrKill: Some of these PIDs are still alive (killing them now): " & $pids)
  for pid in pids: Pid.forceKill(pid)
  if Poll.waitForCondition(allDead()):
    log.warn("terminateOrKill: These PIDs were successfully killed: " & $pids)
    return
  let errMsg: string = "terminateOrKill: Failed to kill PIDs: " & $pids
  DesktopNotification.notify(exe.wenzelsKeyboard, errMsg, urgent = true)
  fail(errMsg)

# Parsing command-line-arguments

const
  noXlibHackOpt: string = "no-xlib-hack"
  keyRepeatedDelayOpt: string = "key-repeated-delay"
  keyRepeatedIntervalOpt: string = "key-repeated-interval"
  xkbLayoutOpt: string = "xkb-layout"
  xkbOptionsOpt: string = "xkb-options"

proc parseCommandLineArgs(): CommandLineArgs =
  let args: cliargs.ParsedArgs = cliargs.parseArgs(
    optsSpec = cliargs.makeOptionsSpec({
      keyRepeatedDelayOpt: cliargs.makeStrOption(
        optRequired = true,
        optPlaceholder = strutils.escape("UINT"),
      ),
      keyRepeatedIntervalOpt: cliargs.makeStrOption(
        optRequired = true,
        optPlaceholder = strutils.escape("UINT"),
      ),
      xkbLayoutOpt: cliargs.makeStrOption(optRequired = true),
      xkbOptionsOpt: cliargs.makeStrOption(optRequired = true),
      noXlibHackOpt: cliargs.makeFlagOption(
        optInfo = "Do not start xlib-keys-hack.",
      ),
      "help": cliargs.makeFlagOption(
        optShort = 'h',
        optInfo = "Show this usage information.",
      ),
    }),
    errWriteLine = proc (line: string): void = writeStderr(line),
    usageDescription = "My keyboard automatic setup utility.",
    usageInfoTransform = proc (usage: string, errors: string): string =
      (if errors.len > 0: "\n" & errors & "\n" else: "") & "\n" & usage & "\n",
    validate = proc (args: cliargs.ParsedArgs, _: string): string =
      var errors: seq[string] = @[]

      if args.positional.len > 0: errors.add(
        "This application does not accept any positional arguments!\n" &
        "Got: " & $args.positional
      )

      # Validate UINT options
      for uintOpt in [keyRepeatedDelayOpt, keyRepeatedIntervalOpt]:
        let str: string = options.get(cliargs.getOptionValue(args, uintOpt).strValue)
        try:
          let x = strutils.parseInt(str)
          if x < 0: raise newException(ValueError, "Not an unsigned integer")
        except ValueError as e:
          errors.add("--" & uintOpt & " value is incorrect (must be an unsigned integer): " & e.msg)

      if errors.len <= 0: "" else: strutils.join(errors, "\n\n")
  )

  CommandLineArgs(
    noXlibHack: cliargs.getOptionValue(args, noXlibHackOpt).flagValue,
    keyRepeatedDelay: strutils.parseInt(
      options.get(cliargs.getOptionValue(args, keyRepeatedDelayOpt).strValue)
    ).uint,
    keyRepeatedInterval: strutils.parseInt(
      options.get(cliargs.getOptionValue(args, keyRepeatedIntervalOpt).strValue)
    ).uint,
    xkbLayout: options.get(cliargs.getOptionValue(args, xkbLayoutOpt).strValue),
    xkbOptions: options.get(cliargs.getOptionValue(args, xkbOptionsOpt).strValue),
  )

# Sub-processes supervision

# Clean all the processes that are potentially still running
proc cleanArtifacts(): void =
  var artifactPids: seq[uint] = @[]
  for x in exe.artifactExecutables:
    let pids: seq[uint] = Pid.findExecutablePids(x)
    log.debug("cleanArtifacts: " & strutils.escape(x) & " PIDs: " & $pids)
    artifactPids.add(pids)
  log.debug("cleanArtifacts: Terminating PIDs: " & $artifactPids)
  Pid.terminateOrKill(artifactPids)

# Take over previous run of `wenzels-keyboard.lock`
proc takeOverPreviousRun(): void =
  const cmd: string = strutils.escape(exe.wenzelsKeyboard)
  log.stage("Taking over of the previous run of " & cmd)

  let pids: seq[uint] = sequtils.filterIt(Pid.findExecutablePids(exe.wenzelsKeyboard), it != pid.uint)
  if pids.len == 0: log.skip("No previous run of " & cmd & " found")
  else: Pid.terminateOrKill(pids)

  log.stage("Cleaning potentially survived artifacts: " & $exe.artifactExecutables)
  cleanArtifacts()

  log.ok("Took over of the previous " & cmd & " run (" & $pids & ")")

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
    log.debug("startSubProcWorkers: Thread starting")

    var threads: seq[ref SubProcWorkerSpec] = @[]
    let noPidYet: options.Option[uint] = options.none(uint)

    block addWorker:
      let cmd: ref Command = (ref Command)(cmd: exe.xbindkeys, args: @["--nodaemon"])
      var spec: ref SubProcWorkerSpec =
        (ref SubProcWorkerSpec)(
          thread: default(Thread[ref Command]),
          subProcPid: noPidYet,
          isRunning: true
        )
      log.debug("startSubProcWorkers: Spawning worker thread for: " & $cmd[])
      createThread(spec.thread, startSubProcWorker, cmd)
      threads.add(spec)

    block addWorker:
      let cmd: ref Command = (ref Command)(cmd: exe.wenzelsXlibKeysHack)
      var spec: ref SubProcWorkerSpec =
        (ref SubProcWorkerSpec)(
          thread: default(Thread[ref Command]),
          subProcPid: noPidYet,
          isRunning: true
        )
      if args.noXlibHack:
        log.debug("startSubProcWorkers: NOT spawning (--no-xlib-hack) worker thread for: " & $cmd[])
        break addWorker
      else:
        log.debug("startSubProcWorkers: Spawning worker thread for: " & $cmd[])
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
        log.debug("startSubProcWorkers: Terminating all the sub processes that are still running")
        var pids: seq[uint] = @[]
        for thread in threads:
          let subProcPid: options.Option[uint] = thread.subProcPid
          if options.isNone(subProcPid):
            log.warn(
              "startSubProcWorkers/terminateAll: No PID reported yet for " & $thread.thread.data[]
            )
          else:
            pids.add(options.get(subProcPid))
        Pid.terminateOrKill(pids)

    template notifyWhenReady(cmd: string): void =
      block ready:
        DesktopNotification.notify(
          exe.wenzelsKeyboard,
          strutils.escape(cmd) & " worker is started",
        )
        for thread in threads:
          if (not thread.isRunning) or options.isNone(thread.subProcPid):
            break ready
        DesktopNotification.notify(
          exe.wenzelsKeyboard,
          "All " & $threads.len & " worker(s) started",
        )

    block eventLoop:
      while true:
        # If no threads are running anymore we are done here
        block areAllDone:
          for thread in threads: (if thread.isRunning: break areAllDone)
          log.debug("startSubProcWorkers: No threads are still running, wrapping things up")
          # Wait for all the threads to be done
          if Poll.waitForCondition(
            block:
              var allThreadsAreDone: bool = true
              for thread in threads: (if running(thread.thread): (allThreadsAreDone = false; break))
              allThreadsAreDone
          ):
            log.error(
              "startSubProcWorkers: Some of the threads seems to be stuck (not joining them)"
            )
          else:
            log.debug("startSubProcWorkers: All threads are done, joining them")
            for thread in threads: joinThread(thread.thread)
          log.debug("startSubProcWorkers: The event loop is done")
          break eventLoop
        let event: ref SubProcWorkerEvent = subProcWorkerEvents.recv()
        case event.kind:
        of started:
          log.debug(
            "startSubProcWorkers: Thread reports PID " & $event.pid &
            " for " & $getEventCmd(event)[]
          )
          updateThread(event, thread): thread.subProcPid = options.some(event.pid)
          notifyWhenReady(getEventCmd(event)[].cmd)
        of exited:
          log.debug(
            "startSubProcWorkers: Thread reports exit code " & $event.exitCode &
            " for " & $getEventCmd(event)[]
          )
          updateThread(event, thread): thread.isRunning = false
          # If any other thread is still running terminate it
          block termination:
            for thread in threads: (if thread.isRunning: (terminateAll(); break termination))
        of failed:
          log.error(
            "startSubProcWorkers: Thread reports failure for " &
            $getEventCmd(event)[] & ": " & strutils.escape(event.message)
          )
          updateThread(event, thread): thread.isRunning = false
          block termination:
            for thread in threads: (if thread.isRunning: (terminateAll(); break termination))
        of cleanup:
          log.debug("startSubProcWorkers: Received cleanup request")
          block termination:
            for thread in threads: (if thread.isRunning: (terminateAll(); break termination))
  finally:
    log.debug("startSubProcWorkers: Reporting subprocesses workers thread finished")
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
  log.debug("terminationSignalWaiter: Thread starting")
  var signal: cint
  let error = posix.sigwait(terminationSigset, signal)
  if error == 0:
    log.debug("terminationSignalWaiter: Received signal: " & $signal)
    mainEvents.send(MainEvent(kind: signalReceived, signal: signal))
  else:
    log.error("terminationSignalWaiter: sigwait failed with code: " & $error)
    mainEvents.send(MainEvent(kind: signalWaitFailed, errorCode: error))

# Main program, stages

# Basic keyboard behavior configurations
proc basicSetup(args: CommandLineArgs): void =
  for command in [
    Command(cmd: exe.xset, args: @["r", "rate", $args.keyRepeatedDelay, $args.keyRepeatedInterval]),
    Command(cmd: exe.setxkbmap, args: @["-layout", args.xkbLayout, "-option", args.xkbOptions]),
    Command(cmd: exe.numlockx, args: @["off"]),
    Command(cmd: exe.numlockx, args: @["on"]),
  ]: SubProc.callCmd(command)

withStderr:
  let xdgRuntimeDir: string = block:
    let envVarName: string = "XDG_RUNTIME_DIR"
    let x: string = envvars.getEnv(envVarName)
    if x == "": fail("Can’t read " & strutils.escape(envVarName))
    x

  let commandLineArgs: CommandLineArgs = parseCommandLineArgs()
  log.debug("Parsed command-line arguments: " & $commandLineArgs)

  # Terminate previous run of wenzels-keyboard and its possible zombified artifacts
  # (or kill those if it they refuse to terminate within a timeout).
  takeOverPreviousRun()

  var
    signalThread: Thread[void]
    workersThread: Thread[CommandLineArgs]
    receivedSignal: options.Option[cint] = options.none(cint)

  try:
    withLockFile(xdgRuntimeDir & "/wenzels-keyboard.lock"):
      DesktopNotification.notify(exe.wenzelsKeyboard, "Setting things up")

      log.stage("Basic setup")
      basicSetup(commandLineArgs)

      log.debug("Opening communications channels")
      subProcWorkerEvents.open()
      mainEvents.open()

      log.debug("Blocking POSIX termination signals (for graceful shutdown)")
      blockTerminationSignals()

      log.stage("Starting termination signal waiter thread")
      createThread(signalThread, terminationSignalWaiter)
      log.stage("Starting subprocesses workers thread")
      createThread(workersThread, startSubProcWorkers, commandLineArgs)

      log.stage("Waiting for a main event to terminate")
      let event = mainEvents.recv()
      case event.kind
      of signalReceived:
        log.stage("Received termination signal (terminating): " & $event.signal)
        receivedSignal = options.some(event.signal)
        DesktopNotification.notify(
          exe.wenzelsKeyboard & " exiting",
          "Received termination signal: " & $event.signal,
        )
      of workersFinished:
        log.warn(
          "Subprocesses workers thread reported finishing " &
          "(probably watched subprocess died, terminating)"
        )
        DesktopNotification.notify(
          exe.wenzelsKeyboard & " exiting",
          "Subprocess worker thread exited early",
          urgent = true,
        )
      of signalWaitFailed:
        DesktopNotification.notify(
          exe.wenzelsKeyboard & " exiting",
          "Termination signal waiting failed",
          urgent = true,
        )
        fail("Signal waiter failed with error code: " & $event.errorCode)

  finally:
    log.stage("Cleaning up")

    log.debug("Sending clean up event to subprocesses workers thread")
    subProcWorkerEvents.send((ref SubProcWorkerEvent)(kind: cleanup))
    log.debug("Waiting for the subprocesses workers thread to finish")
    if Poll.waitForCondition(not running(workersThread)):
      log.debug("Joining the subprocesses workers thread")
      joinThread(workersThread)
    else:
      log.error("Failed to cleanup subprocesses workers thread (ignoring it)")

    log.debug("Triggering SIGUSR1 to make sure termination signal waiter thread is finishing")
    if posix.kill(pid, posix.SIGUSR1) != 0: os.raiseOSError(os.osLastError())
    log.debug("Waiting for the termination signal waiter thread to finish")
    if Poll.waitForCondition(not running(signalThread)):
      log.debug("Joining the termination signal waiter thread")
      joinThread(signalThread)
    else:
      log.error("Failed to cleanup termination signal waiter thread (ignoring it)")

    log.debug("Closing communication channels")
    subProcWorkerEvents.close()
    mainEvents.close()

    log.debug("Cleaning up artifacts if any left (normally there should be none)")
    cleanArtifacts()

  log.stage("Final exiting")
  if options.isNone(receivedSignal):
    log.error("Unexpected exit, without a termination signal (probably watched process died)")
    quit 1
  else:
    # Conventional shell exit status: 128 + signal number.
    let finalSignal: int = 128 + options.get(receivedSignal)
    log.ok("Received signal " & $options.get(receivedSignal) & ", exiting with " & $finalSignal)
    quit finalSignal
