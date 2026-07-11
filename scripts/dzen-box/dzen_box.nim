# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# Show a small Dzen2 box window in the corner of the (pseudo-)primary screen for
# few seconds.
#
# Accepts only text (which is supposed to be no longer than 2–4 chars longs) and
# optional foreground color for it.
#
# Useful for rendering current audio volume or screen brightness value when a
# hot key is pressed to change it. Or to indicate when Caps Lock is pressed for
# example.
#
# Usage examples:
#
#   dzen-box FOO
#   dzen-box 50% blue
#   dzen-box 100% yellow

from std/os import commandLineParams, raiseOSError, OSErrorCode, sleep, fileExists, removeFile
from std/options import Option, none, some, isNone, isSome, get
from std/posix import Pid, kill, errno, getpid, SIGUSR1, ESRCH
from std/strutils import escape, strip, parseUInt
from std/osproc import Process, inputStream, waitForExit
from std/streams import Stream, writeLine, flush, close
from std/envvars import getEnv
from std/net import Socket, newSocket, bindUnix, listen, accept, close, connectUnix, send, recvLine

from log as logging import Log, defaultTimeFormat, fail, debug, skip, ok, warn, info, stage, error
from needexe import checkExecutableDependencies
from stderr import CustomStderrWriter, withStderr, writeLogLine
from subproc import Command, StartCmdReturnType, startCmd
from poll import waitForCondition
from lock import FileLock, acquireFileLock, releaseFileLock
from signals import blockTerminationSignals, terminationSignals, waitForTerminationSignal
from either import Either
from timerfd import TimerFd, newTimerFd, arm, disarm, wait, close

let pid: Pid = getpid()

checkExecutableDependencies()

const logTimeFormat: string = defaultTimeFormat
type LogWriter = CustomStderrWriter
type LogType = Log[logTimeFormat, LogWriter]

let log = block:
  var x = LogType()
  # DZEN_BOX_SILENT=1 to only log fatal failures
  if "DZEN_BOX_SILENT".getEnv() == "1": logging.setNoisinessLevel(x, logging.failLevel)
  x

template fail(msg: string, exitCode: int = 1): void = (log.fail(msg); quit exitCode)

let
  xdgRuntimeDir: string =
    withStderr:
      let envVarName: string = "XDG_RUNTIME_DIR"
      let x: string = envVarName.getEnv
      if x == "": fail("Can’t read " & envVarName.escape)
      x

  displayArgs: seq[string] = block:
    let displayNumFile: string = xdgRuntimeDir & "/pseudo-primary-display"
    if displayNumFile.fileExists:
      let displayNum: uint = displayNumFile.readFile.strip.parseUInt
      @["-xs", $displayNum]
    else:
      @[]

  ipcSocketFile: string = xdgRuntimeDir & "/dzen-box.ipc.sock"
  lockFile: string = xdgRuntimeDir & "/dzen-box.lock"

const
  # For how long the dzen2 window stays visible
  wndShowingSeconds: uint = 1

  wndTitle: string = "dzen-box"
  bgColor: string = "black"
  defaultFgColor: string = "white"

  fontFamily: string = "Hack"
  fontStyle: string = "bold"

  wndWidth: uint = 120
  wndHeight: uint = 120

  wndX: int = static(block:
    var value: int = -100
    if value < 0: value - wndWidth.int else: value
  )

  wndY: int = static(block:
    var value: int = 100
    if value < 0: value - wndHeight.int else: value
  )

  dzen2BaseArgs = [
    "-ta", "c", # Text align: center
    "-title-name", wndTitle,
    "-w", $wndWidth, "-h", $wndHeight,
    "-x", $wndX, "-y", $wndY,
    "-bg", bgColor
  ]

template mkFontStr(fontSize: uint = 8): string =
  ("-*-" & fontFamily & "-" & fontStyle & "-*-*-*-" & $fontSize & "-*-*-*-*-*-*-*")

# Pick a font size based on the text length, making sure it fits the fixed size window box
template getFontSize(text: string): uint =
  if text.len <= 2: 70
  elif text.len == 3: 42
  elif text.len == 4: 32
  else: raiseAssert("Unexpected length (" & $text.len & ") of text: " & text)

# Make prepared command-line arguments for dzen2
template mkDzenArgs(fgColor: string, fontStr: string): seq[string] =
  block:
    var result: seq[string] = @dzen2BaseArgs
    result.add @["-fg", fgColor, "-fn", fontStr]
    result.add displayArgs
    result

# Make a line to write to stdin of dzen2 sub-process
template mkDzenLine(fgColor: string, text: string): string =
  ("^fn(" & text.getFontSize.mkFontStr & ")^fg(" & fgColor & ")" & text)

template parseArgs(): tuple[text: string, fgColor: string] =
  block:
    let args: seq[string] = commandLineParams()
    if args.len <= 0 or args.len > 2: fail("Incorrect amount of arguments: " & $args.len)
    (args[0], if args.len < 2 or args[1].len == 0: defaultFgColor else: args[1])

type
  MainEventKind = enum
    # Termination signal waiter thread
    terminationRequest
    terminationThreadFailed

    # IPC server thread
    ipcDzen2LineWrite

    # dzen2 thread
    dzen2Exited
    dzen2Failed

    # Timer waiter thread
    timerExpired
    timerFailed

  MainEvent {.requiresInit.} = object
    case kind: MainEventKind

    # Termination signal waiter thread
    of terminationRequest: signal: cint
    of terminationThreadFailed: errMsg: string

    # IPC server thread
    of ipcDzen2LineWrite:
      dzen2Line: string
      reply: ptr Channel[tuple[]]

    # dzen2 thread
    of dzen2Exited: dzen2ExitCode: int
    of dzen2Failed: dzen2FailureMsg: string

    # Timer waiter thread
    of timerExpired: discard
    of timerFailed: timerFailureMsg: string

var mainEvenChannel: Channel[ref MainEvent]

# Send `none` to terminate dzen2 and finish the thread
var dzen2EventChannel: Channel[Option[string]]

proc startDzen2(command: Command): StartCmdReturnType =
  log.debug("startDzen2: Starting dzen2")
  startCmd(
    log,
    command,
    resetSignals = @["TERM"],
    createStreams = true,
    forwardStdout = true,
    forwardStderr = true,
    logPrefix = command.cmd.escape,
  )

# A dzen2 stdin writer thread (read events though a `Channel` and send lines to dzen2)
proc dzen2Writer(process: StartCmdReturnType): void {.thread.} =
  let stream: Stream = process.p.inputStream
  log.debug("dzen2Writer: Starting dzen2 events handling loop")
  try:
    while true:
      let event = dzen2EventChannel.recv
      if event.isSome:
        log.debug("dzen2Writer: writing dzen2 line: " & event.get.escape)
        stream.writeLine event.get
        stream.flush
      else:
        log.debug("dzen2Writer: received termination request (finishing thread)")
        break
  except IOError:
    log.debug("dzen2Writer: stdin seems to be closed (finishing thread)")
  except CatchableError as e:
    fail("dzen2Writer: Unexpected exception: " & e.msg)
  finally:
    process.close()

proc dzen2TerminationGuard(process: StartCmdReturnType): void {.thread.} =
  log.debug "dzen2TerminationGuard: Thread is starting"
  try:
    let exitCode: int = process.p.waitForExit
    log.debug("dzen2TerminationGuard: dzen2 exited with exit code: " & $exitCode)
    mainEvenChannel.send((ref MainEvent)(kind: dzen2Exited, dzen2ExitCode: exitCode))
  except CatchableError as e:
    log.error("dzen2TerminationGuard: Thread failed with: " & e.msg)
    mainEvenChannel.send((ref MainEvent)(kind: dzen2Failed, dzen2FailureMsg: e.msg))

const
  IPC_SERVER_OK_MESSAGE: string = "OK"
  IPC_SERVER_SHUTDOWN_MESSAGE = "SHUTDOWN"
  IPC_SERVER_SHUTDOWN_OK_MESSAGE: string = "OK, SHUTTING DOWN"

proc tryToSendToExistingDzen2(socketPath: string, dzenLine: string): bool =
  var client: Socket = newSocket(net.AF_UNIX, net.SOCK_STREAM, net.IPPROTO_IP)
  log.debug(
    "tryToSendToExistingDzen2: " &
    "Trying to send a dzen2 line to an existing dzen-box instance through socket " &
    socketPath.escape & ": " & dzenLine
  )
  try:
    client.connectUnix socketPath # Fails if no listener
    client.send(dzenLine & "\n")
    let response: string = client.recvLine
    # The dzen-box listener must respond that it accepted it
    result = response == IPC_SERVER_OK_MESSAGE
    if result:
      log.debug(
        "tryToSendToExistingDzen2: Successfully sent dzen2 line to the existing dzen-box instance"
      )
    else:
      fail(
        "tryToSendToExistingDzen2: Response was not " & IPC_SERVER_OK_MESSAGE.escape &
        " from the existing dzen-box instance through socket, got instead: " &
        response.escape
      )
  except CatchableError as error:
    log.debug(
      "tryToSendToExistingDzen2: " &
      "It seems there is no existing dzen-box instance to receive the dzen2 line: " &
      error.msg
    )
    result = false # No listener, attempt not successful (need to start own dzen2 instance)
  finally:
    client.close

proc terminateIpcServer(socketPath: string): void =
  var client: Socket = newSocket(net.AF_UNIX, net.SOCK_STREAM, net.IPPROTO_IP)
  log.debug(
    "terminateIpcServer: Terminating IPC server by sending " &
    IPC_SERVER_SHUTDOWN_MESSAGE.escape & " message to " & socketPath.escape
  )
  try:
    client.connectUnix socketPath
    client.send(IPC_SERVER_SHUTDOWN_MESSAGE & "\n")
    let response: string = client.recvLine
    if response == IPC_SERVER_SHUTDOWN_OK_MESSAGE:
      log.debug(
        "terminateIpcServer: Successfully sent " &
        IPC_SERVER_SHUTDOWN_MESSAGE.escape & " message to IPC server"
      )
    else:
      fail(
        "terminateIpcServer: Response for " & IPC_SERVER_SHUTDOWN_MESSAGE.escape &
        " message was not " & IPC_SERVER_SHUTDOWN_OK_MESSAGE.escape &
        " got instead: " & response.escape
      )
  except CatchableError as error:
    fail(
      "terminateIpcServer: Failed to send " & IPC_SERVER_SHUTDOWN_MESSAGE.escape &
      " meesage to IPC server: " & error.msg
    )

  finally:
    client.close

# Try to connect to a socket to see if it is healthy
proc tryConnect(socketPath: string): bool {.inline.} =
  var client: Socket = newSocket(net.AF_UNIX, net.SOCK_STREAM, net.IPPROTO_IP)
  try: (client.connectUnix socketPath; return true)
  except CatchableError: return false
  finally: client.close

proc createSocketServer(socketPath: string): Socket =
  # Remove stale socket only after checking nobody answers there
  if socketPath.fileExists and not socketPath.tryConnect:
    log.warn("createSocketServer: Removing stale socket file: " & socketPath.escape)
    socketPath.removeFile
  log.debug("createSocketServer: Creating new socket server for socket file: " & socketPath.escape)
  var server = newSocket(net.AF_UNIX, net.SOCK_STREAM, net.IPPROTO_IP)
  try: (server.bindUnix socketPath; server.listen; return server)
  except CatchableError: (server.close; raise)

proc runIpcServer(server: Socket): void {.thread.} =
  log.debug "runIpcServer: Starting IPC server client waiting loop"

  while true:
    var client: owned Socket
    try:
      server.accept client
      let line: string = client.recvLine

      if line == IPC_SERVER_SHUTDOWN_MESSAGE:
        log.info(
          "runIpcServer: Received " & IPC_SERVER_SHUTDOWN_MESSAGE.escape &
          " (responding to the client with " & IPC_SERVER_SHUTDOWN_OK_MESSAGE.escape &
          " and shutting down IPC server)"
        )
        client.send(IPC_SERVER_SHUTDOWN_OK_MESSAGE & "\n")
        break

      log.debug(
        "runIpcServer: Received dzen2 line via IPC: " & line.escape &
        " (reporting the update to the main thread)"
      )

      var reply: Channel[tuple[]]
      reply.open 1
      defer: reply.close

      mainEvenChannel.send (ref MainEvent)(
        kind: ipcDzen2LineWrite,
        dzen2Line: line,
        reply: reply.addr,
      )

      log.debug("runIpcServer: Waiting for reply from the main thread")
      discard reply.recv

      log.debug("runIpcServer: Responding to the client with " & IPC_SERVER_OK_MESSAGE.escape)
      client.send(IPC_SERVER_OK_MESSAGE & "\n")

    except CatchableError as error:
      # Not stopping here, if client broke just wait for another one
      log.error("runIpcServer: Exception: " & error.msg)

    finally:
      log.debug("runIpcServer: Closing the client")
      client.close

proc terminationSignalWaiter(): void {.thread.} =
  log.debug "terminationSignalWaiter: Thread starting"
  let signal: Either[string, cint] = waitForTerminationSignal(log)
  if signal.isRight:
    log.debug("terminationSignalWaiter: Received signal: " & $signal.right)
    mainEvenChannel.send((ref MainEvent)(kind: terminationRequest, signal: signal.right))
  else:
    log.error("terminationSignalWaiter: Thread failed: " & signal.left)
    mainEvenChannel.send((ref MainEvent)(kind: terminationThreadFailed, errMsg: signal.left))

proc timerWaiter(timer: TimerFd): void {.thread.} =
  log.debug "timerWaiter: Thread starting"
  try:
    timer.wait
    log.error("timerWaiter: Timer expired (reporting)")
    mainEvenChannel.send (ref MainEvent)(kind: timerExpired)
  except CatchableError as error:
    log.error("timerWaiter: Exception: " & error.msg)
    mainEvenChannel.send (ref MainEvent)(kind: timerFailed, timerFailureMsg: error.msg)

withStderr:
  log.stage "Parsing command-line arguments"
  let (text, fgColor) = parseArgs

  let dzen2Line: string = mkDzenLine(fgColor, text)

  var
    terminationSignalWaiterThread: Thread[void]
    terminationSignalWaiterThreadStarted: bool = false

    dzen2Thread: Thread[StartCmdReturnType]
    dzen2ThreadStarted: bool = false

    dzen2TerminationGuardThread: Thread[StartCmdReturnType]
    dzen2TerminationGuardThreadStarted: bool = false

    ipcServer: Option[Socket] = Socket.none
    ipcServerThread: Thread[Socket]
    ipcServerThreadStarted: bool = false

    timer: TimerFd = newTimerFd()
    timerWaiterThread: Thread[TimerFd]
    timerWaiterThreadStarted: bool = false

    lock: Option[FileLock] = FileLock.none

    mainExitCode: int = 0

  log.info("Blocking termination signals: " & $terminationSignals)
  blockTerminationSignals()

  log.debug "Opening cross-thread communication channels"
  mainEvenChannel.open
  dzen2EventChannel.open

  # `bool` indicates success (`false` means a retry)
  template tryExistingInstance(): bool =
    block:
      log.stage "Trying to send dzen2 line to an existing dzen-box instance"
      var result: bool = tryToSendToExistingDzen2(ipcSocketFile, dzen2Line)
      if result: log.ok("Successfully re-used existing dzen-box instance")
      result

  # `bool` indicates success (`false` means a retry)
  template tryNewInstance(): bool =
    block:
      log.stage "Starting own dzen2 instance"

      log.debug("Trying to acquire file lock " & lockFile.escape)
      lock = lockFile.acquireFileLock
      if lock.isNone:
        lock = FileLock.none
        log.info(
          "Couldn’t acquire lock " & lockFile.escape & ", it seems there " &
          " is another dzen-box instance holding it (retrying)"
        )
        false
      else:
        log.info("Successfully acquired file lock: " & lockFile.escape)

        log.stage "Starting IPC server (creating socket)"
        let server: Socket = ipcSocketFile.createSocketServer
        ipcServer = server.some

        log.stage "Starting termination signal waiter thread"
        createThread(terminationSignalWaiterThread, terminationSignalWaiter)
        terminationSignalWaiterThreadStarted = true

        log.stage "Starting IPC server thread"
        createThread(ipcServerThread, runIpcServer, server)
        ipcServerThreadStarted = true

        log.stage "Starting dzen2 process"
        let dzen2Process: StartCmdReturnType =
          startDzen2(Command(cmd: needexe.dzen2, args: mkDzenArgs(fgColor, mkFontStr())))

        log.stage "Starting dzen2 thread"
        createThread(dzen2Thread, dzen2Writer, dzen2Process)
        dzen2ThreadStarted = true

        log.stage "Starting dzen2 termination guard thread"
        createThread(dzen2TerminationGuardThread, dzen2TerminationGuard, dzen2Process)
        dzen2TerminationGuardThreadStarted = true

        log.debug("Sending dzen2 line to dzen2 events channel: " & dzen2Line.escape)
        dzen2EventChannel.send dzen2Line.some

        log.debug "Starting the timer waiter thread"
        createThread(timerWaiterThread, timerWaiter, timer)
        timerWaiterThreadStarted = true

        log.debug("Arming the timer to " & $wndShowingSeconds & " second(s)")
        timer.arm(wndShowingSeconds * 1000)

        log.ok("New dzen-box instance started successfully")
        true

  template runMainEventLoop(): void =
    block mainEventLoop:
      while true:
        let event = mainEvenChannel.recv()
        case event.kind

        of terminationRequest:
          mainExitCode = 128 + event.signal
          log.info(
            "Termination requested by signal: " & $event.signal &
            " (setting exit code to 128 + signal number: " & $mainExitCode & ")"
          )
          break mainEventLoop

        of terminationThreadFailed:
          log.error("Termination signal listener thread failed: " & event.errMsg)
          mainExitCode = 1
          break mainEventLoop

        of ipcDzen2LineWrite:
          log.debug "Disarming the timer"
          timer.disarm

          log.debug "Forwarding dzen2 line from IPC server to dzen2 events channel"
          dzen2EventChannel.send event.dzen2Line.some
          log.debug("Replying to the IPC server with: " & IPC_SERVER_OK_MESSAGE.escape)
          event.reply[].send () # Reporting successful handling

          log.debug("Re-arming the timer to " & $wndShowingSeconds & " second(s)")
          timer.arm(wndShowingSeconds * 1000)

        of dzen2Exited:
          log.debug("dzen2 exited with exit code: " & $event.dzen2ExitCode)
          mainExitCode = event.dzen2ExitCode
          break mainEventLoop

        of dzen2Failed:
          log.error("dzen2 termination guard thread failed with: " & event.dzen2FailureMsg)
          mainExitCode = 1
          break mainEventLoop

        of timerExpired:
          log.debug "Timer expired"
          break mainEventLoop

        of timerFailed:
          log.error("Timer waiter thread failed with: " & event.timerFailureMsg)
          mainExitCode = 1
          break mainEventLoop

  try:
    # Retry loop
    while true:
      if tryExistingInstance(): break
      elif tryNewInstance():
        log.stage "Running main events loop"
        runMainEventLoop
        break

  except CatchableError as error:
    log.error("Main event loop exception: " & error.msg)
    mainExitCode = 1

  finally:
    log.stage "Cleaning up"

    type KillArg = tuple[pid: Pid, signal: cint]

    template threadCleanup[T](
      killing: Option[KillArg] = KillArg.none,
      thread: Thread[T],
      threadName: string,
    ): void =
      block:
        if killing.isSome:
          if kill(killing.get.pid, killing.get.signal) != 0:
            let error: cint = errno
            case errno
            of ESRCH: discard # Not alive, considering a success
            else: raiseOSError(OSErrorCode(error))
        log.debug("Waiting for " & threadName.escape & " thread to finish")
        if waitForCondition(not thread.running):
          log.debug("Joining " & threadName.escape & " thread")
          thread.joinThread
          log.ok("Successfully cleaned up " & threadName.escape & " thread")
        else:
          log.error("Failed to cleanup " & threadName.escape & " thread (ignoring it)")

    if terminationSignalWaiterThreadStarted:
      log.debug("Triggering SIGUSR1 to make sure termination signal waiter thread is finishing")
      threadCleanup(
        killing = (pid: pid, signal: SIGUSR1).some,
        thread = terminationSignalWaiterThread,
        threadName = "termination signal waiter",
      )

    if dzen2ThreadStarted:
      log.debug("Sending termination event to dzen2 events channel")
      dzen2EventChannel.send string.none
      threadCleanup(thread = dzen2Thread, threadName = "dzen2")

    if dzen2TerminationGuardThreadStarted:
      # dzen2 process should be terminated by the `dzen2Thread` above
      log.debug("Waiting for the dzen2 termination guard thread to finish")
      threadCleanup(thread = dzen2TerminationGuardThread, threadName = "dzen2 termination guard")

    if ipcServerThreadStarted:
      if ipcServerThread.running:
        log.debug("Terminating IPC server thread")
        terminateIpcServer ipcSocketFile
        threadCleanup(thread = ipcServerThread, threadName = "IPC server")
      else:
        log.debug("IPC server thread is not running (only joining the thread)")
        ipcServerThread.joinThread

    if timerWaiterThreadStarted:
      log.debug(
        "Waking the timer waiting thread by arming the timer to 1 ms so that the thread finishes"
      )
      timer.arm 1
      threadCleanup(thread = timerWaiterThread, threadName = "Timer waiter")

    if ipcServer.isSome:
      log.debug("Closing IPC server: " & ipcSocketFile.escape)
      ipcServer.get.close
      ipcSocketFile.removeFile

    if lock.isSome:
      log.debug("Releasing file lock: " & lockFile.escape)
      lock.get.releaseFileLock

    log.debug "Closing the timer"
    timer.close

    log.debug "Closing cross-thread communication channels"
    dzen2EventChannel.close
    mainEvenChannel.close

  log.stage("Final exit (exit code: " & $mainExitCode & ")")
  quit mainExitCode
