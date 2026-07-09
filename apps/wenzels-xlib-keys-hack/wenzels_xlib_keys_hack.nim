# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# Generic xinput pointer setup script.

from std/options import Option, some, none, isNone, isSome, get
from std/os import raiseOSError, osLastError, OSErrorCode, quoteShell
from std/osproc import Process, close, waitForExit, processID, inputStream, outputStream, errorStream
from std/posix import access, R_OK, kill, SIGUSR1, SIGTERM, SIGINT, Pid, getpid, errno
from std/streams import Stream, close
from std/strutils import join, startsWith, escape
from std/tables import `[]`, `[]=`, OrderedTable, toOrderedTable, pairs

from cliargs import ArgOptions, makeOptionsSpec, makeFlagOption, makeStrOption
from desktopnotifications import notify
from either import Either
from log as logging import Log, defaultTimeFormat, fail, debug, skip, ok, warn, info, stage, error
from needexe import checkExecutableDependencies
from poll import waitForCondition
from signals import waitForTerminationSignal, blockTerminationSignals, terminationSignals
from stderr import CustomStderrWriter, withStderr, writeLogLine
from subproc import Command, startCmd, callCmd, redirectStreamToLog

checkExecutableDependencies()

let pid: Pid = getpid()

const logTimeFormat: string = defaultTimeFormat
type LogWriter = CustomStderrWriter
type LogType = Log[logTimeFormat, LogWriter]
let log = LogType()

template fail(msg: string, exitCode: int = 1): void = (log.fail(msg); quit exitCode)

type
  MainEventKind = enum
    started
    exited
    failed
    terminationThreadFailed
    terminationRequest

  MainEvent {.requiresInit.} = object
    case kind: MainEventKind

    # xlib-keys-hack thread related
    of started: pid: uint
    of exited: exitCode: int
    of failed: message: string

    # Termination signal waiter thread related
    of terminationRequest: signal: cint
    of terminationThreadFailed: errMsg: string

var mainEvents: Channel[ref MainEvent]

proc terminationSignalWaiter(log: LogType): void {.thread.} =
  log.debug "terminationSignalWaiter: Thread starting"
  let signal: Either[string, cint] = waitForTerminationSignal(log)
  if signal.isRight:
    log.debug("terminationSignalWaiter: Received signal: " & $signal.right)
    mainEvents.send((ref MainEvent)(kind: terminationRequest, signal: signal.right))
  else:
    log.debug("terminationSignalWaiter: Thread failed: " & signal.left)
    mainEvents.send((ref MainEvent)(kind: terminationThreadFailed, errMsg: signal.left))

proc startXlibKeysHack(xlibKeysHackArgs: seq[string]): void {.thread.} =
  var
    p: osproc.Process = nil
    stderrThread: Thread[tuple[log: LogType, stream: Stream, prefix: string]]
    stdoutThread: Thread[tuple[log: LogType, stream: Stream, prefix: string]]

  try:
    p = startCmd(
      log,
      Command(cmd: needexe.xlibKeysHack, args: xlibKeysHackArgs),
      resetSignals = @["TERM"],
      createStreams = true,
    )

    p.inputStream.close
    let pfx: string = needexe.xlibKeysHack & " "
    createThread(stderrThread, redirectStreamToLog, (log, p.errorStream, pfx & "stderr"))
    createThread(stdoutThread, redirectStreamToLog, (log, p.outputStream, pfx & "stdout"))

    let pid: uint = p.processID.uint
    mainEvents.send((ref MainEvent)(kind: started, pid: pid))

    let exitCode: int = p.waitForExit
    mainEvents.send((ref MainEvent)(kind: exited, exitCode: exitCode))

  # Prevent exceptions from propagating to the main thread causing the app to ungracefully fail
  except CatchableError as error:
    mainEvents.send((ref MainEvent)(kind: failed, message: error.msg))
  finally:
    # Cleanup the process if it was started successfully
    if p != nil:
      try:
        p.close
        stderrThread.joinThread
        stdoutThread.joinThread
      except CatchableError:
        discard

# Trigger release events for all currently pressed keys.
#
# When “xlib-keys-hack” is terminated before it has a chance to process the key
# release event the key can get stuck and repeat indefinitely until you press
# the key again. `setsid` prevents it from being interrupted (e.g. by SIGINT).
proc cleanupPressedKeys(log: LogType): void =
  const
    xinput: string = quoteShell(needexe.xinput)
    sed: string = quoteShell(needexe.sed)
    sort: string = quoteShell(needexe.sort)
    xargs: string = quoteShell(needexe.xargs)
    xdotool: string = quoteShell(needexe.xdotool)
  let dashShCode: string = """
    """ & xinput & """ list --short |
      """ & sed & """ -n 's/.*id=\([0-9][0-9]*\).*\[slave[[:space:]]*keyboard.*/\1/p' |
      while IFS= read -r id; do """ & xinput & """ query-state "$id" 2>/dev/null || :; done |
      """ & sed & """ -n 's/^[[:space:]]*key\[\([0-9][0-9]*\)\]=down$/\1/p' |
      """ & sort & """ -nu |
      """ & xargs & """ -r """ & xdotool & """ keyup || :
  """
  callCmd(log, Command(cmd: needexe.setsid, args: @["--wait", needexe.dash, "-c", dashShCode]))

const
  gamingOpt: string = "gaming"
  shiftNumericKeysOpt: string = "shift-numeric-keys"
  forceRegularOpt: string = "force-regular"

let
  args: cliargs.ParsedArgs = cliargs.parseArgs(
    makeOptionsSpec(static {
      gamingOpt: makeFlagOption(
        optInfo = [
          "Enable gaming keyboard mode.",

          "When gaming mode is on “additional controls” feature is disabled " &
          "so those keys (typically Caps Lock and Enter) will be triggered " &
          "immediately when they are just pressed.",

          "Also on a regular qwerty keyboard it’s more useful to have Caps Lock " &
          "key instead of remapped Escape on its place so you can still have " &
          "Escape for whatever purpuse in a game. And in some games you can’t " &
          "even map Escape as you would map any other key since it’s reserved " &
          "to call game menu for instance.",

          "Also “Super double press” feature (to switch-on “alternative mode”) " &
          "may stand in your way if you press Super key too often in a game",
        ].join("\n\n"),
      ),
      shiftNumericKeysOpt: makeFlagOption(
        optInfo = [
          "Shift numeric keys (numbers row) one key to the right, and move " &
          "minus/dash key to the left side at '1' key position.",

          "Makes sense for regular qwerty keyboards with askew key columns. " &
          "Makes the key columns feel more centered.",
        ].join("\n\n")
      ),
      forceRegularOpt: makeFlagOption(
        optInfo = [
          "Force regular keyboard configuration.",

          "Even in presence of keyboards like ErgoDox EZ enforce use of " &
          "settings for the regular QWERTY keyboard.",
        ].join("\n\n")
      ),
      "help": makeFlagOption(
        optShort = 'h',
        optInfo = "Show this usage information.",
      ),
    }),
    usageDescription = "My setup script for my xlib-keys-hack keyboard utility.",
    usageInfoTransform = proc (usage: string, errors: string): string =
      (if errors.len > 0: "\n" & errors & "\n" else: "") & "\n" & usage & "\n",
  )

  gamingModeEnabled: bool = cliargs.getOptionValue(args, gamingOpt).flagValue
  numericRowShifted: bool = cliargs.getOptionValue(args, shiftNumericKeysOpt).flagValue
  regularKeyboardConfigForced: bool = cliargs.getOptionValue(args, forceRegularOpt).flagValue

type
  Keyboard {.requiresInit.} = object
    # It is a regular QWERTY keyboard (typical QWERTY layout with askew key columns)
    isRegular: bool = true

    # This keyboard can be placed on top of the laptop embedded keyboard
    canBePlacedOnTopOfLaptopKeyboard: bool = false

    # Force preservation of “Additional Controls” feature (Caps Lock and Enter keys).
    # “Additional Controls” are disabled for non-regular keyboard configuration
    # but some non-regular 60% keyboards have layouts that benefit from this feature.
    forceAdditionalControls: bool = false

    # Some non-regular 60% keyboards have Escape where the Caps Lock is usually is,
    # right before the A key. That layout benefits from treating this Escape key
    # as part of “Additional Controls” feature.
    forceEscapeAsAdditionalControl: bool = false

    disables: seq[string] = @[]
    devices: seq[string] = @[]

const embeddedLaptopKeyboardKey: string = "Embedded laptop keyboard"

# Pre-compute some values in compile-time
var (keyboardsMap, xlibKeysHackArgs) = static(block:
  let keyboardsMap: OrderedTable[string, Keyboard] = toOrderedTable[string, Keyboard]({
    embeddedLaptopKeyboardKey: Keyboard(
      disables: @[ "AT Translated Set 2 keyboard" ],
      devices: @[ "/dev/input/by-path/platform-i8042-serio-0-event-kbd" ],
    ),

    "Corsair K63 mechanical gaming keyboard": Keyboard(
      disables: @[
        "Corsair Corsair Gaming K63 Keyboard",
        "Corsair Corsair Gaming K63 Keyboard Keyboard",
        "Corsair Corsair Gaming K64 Keyboard Consumer Control",
      ],
      devices: @[
        "/dev/input/by-id/usb-Corsair_Corsair_Gaming_K63_Keyboard_0B008012AF0C1D2558ED9875F5001945-event-if01",
        "/dev/input/by-id/usb-Corsair_Corsair_Gaming_K63_Keyboard_0B008012AF0C1D2558ED9875F5001945-event-kbd",
        "/dev/input/by-id/usb-Corsair_Corsair_Gaming_K63_Keyboard_0B008012AF0C1D2558ED9875F5001945-if01-event-kbd",
      ],
    ),

    "Oklick 910G mechanical gaming keyboard": Keyboard(
      disables: @[
        "HID 04b4:6018",
        "HID 04b4:6018 System Control",
        "HID 04b4:6018 Keyboard",
        "HID 04b4:6018 Consumer Control",

        # It creates some pointers but I don’t think there is any mouse control
        # on the keyboard. Maybe there are some macroses, just disabling those
        # since I don’t use it.
        "HID 04b4:6018 Mouse",
        "HID 04b4:6018 Consumer Control",
      ],
      devices: @[
        "/dev/input/by-id/usb-04b4_6018-event-if01",
        "/dev/input/by-id/usb-04b4_6018-event-kbd",

        # "/dev/input/by-id/usb-04b4_6018-if01-mouse",
        # "/dev/input/by-id/usb-04b4_6018-if01-event-mouse",
      ],
    ),

    "JetAccess SLIM LINE K9 wireless keyboard": Keyboard(
      canBePlacedOnTopOfLaptopKeyboard: true,
      disables: @[ "Telink Wireless Receiver" ],
      devices: @[
        "/dev/input/by-id/usb-Telink_Wireless_Receiver-event-kbd",
        "/dev/input/by-id/usb-Telink_Wireless_Receiver-if01-event-mouse",
        "/dev/input/by-id/usb-Telink_Wireless_Receiver-if01-mouse",
      ],
    ),

    "Ducky One 2 Mini RGB keyboard": Keyboard(
      canBePlacedOnTopOfLaptopKeyboard: true,
      disables: @[ "Ducky Ducky One2 Mini RGB" ],
      devices: @[
        "/dev/input/by-id/usb-Ducky_Ducky_One2_Mini_RGB_DK-V1.08-190201-event-if03",
        "/dev/input/by-id/usb-Ducky_Ducky_One2_Mini_RGB_DK-V1.08-190201-event-kbd",
        "/dev/input/by-id/usb-Ducky_Ducky_One2_Mini_RGB_DK-V1.08-190201-if01-event-mouse",
        "/dev/input/by-id/usb-Ducky_Ducky_One2_Mini_RGB_DK-V1.08-190201-if01-mouse",
        "/dev/input/by-id/usb-Ducky_Ducky_One2_Mini_RGB_DK-V1.08-190201-if02-event-kbd",
      ],
    ),

    "ZSA ErgoDox EZ keyboard": Keyboard(
      isRegular: false,
      canBePlacedOnTopOfLaptopKeyboard: true,
      disables: @[
        # Glow
        "ZSA Technology Labs Inc ErgoDox EZ Glow",
        "ZSA Technology Labs Inc ErgoDox EZ Glow Keyboard",
        # Shine
        "ZSA Technology Labs Inc ErgoDox EZ Shine",
        "ZSA Technology Labs Inc ErgoDox EZ Shine Keyboard",
      ],
      devices: @[
        # Glow
        "/dev/input/by-id/usb-ZSA_Technology_Labs_Inc_ErgoDox_EZ_Glow-event-kbd",
        # Shine
        "/dev/input/by-id/usb-ZSA_Technology_Labs_Inc_ErgoDox_EZ_Shine-event-kbd",
      ],
    ),

    "ZSA Moonlander Mark I keyboard": Keyboard(
      isRegular: false,
      canBePlacedOnTopOfLaptopKeyboard: true,
      disables: @[
        "ZSA Moonlander Mark I",
        "ZSA Moonlander Mark I Keyboard",
      ],
      devices: @[
        "/dev/input/by-id/usb-ZSA_Moonlander_Mark_I-event-kbd",
      ],
    ),

    "ZSA Planck EZ keyboard": Keyboard(
      isRegular: false,
      canBePlacedOnTopOfLaptopKeyboard: true,
      # Planck is 60%, it’s more convenient with “Additional Controls” feature.
      forceAdditionalControls: true,
      # On Planck Escape key is at the place where Caps Lock usually is.
      # Treating it as an “Additional Control”.
      forceEscapeAsAdditionalControl: true,
      disables: @[
        "ZSA Technology Labs Planck EZ Glow",
        "ZSA Technology Labs Planck EZ Glow Keyboard",
      ],
      devices: @[
        "/dev/input/by-id/usb-ZSA_Technology_Labs_Planck_EZ_Glow-event-kbd",
      ],
    ),
  })

  var
    xlibKeysHackArgs: seq[string] = block:
      var argsAcc: seq[string] = @[]

      for k, v in keyboardsMap.pairs:
        for disable in v.disables: argsAcc.add("--disable-xinput-device-name=" & disable)

        # Embedded keyboard is disabled (this devices list is not added to the
        # xlib-keys-hack arguments) when there is an external keyboard.
        # Because of two reasons:
        #
        #   1. The configuration for external keyboard can be pretty different and
        #      thus incompatible (e.g. “conventional” QWERTY keyboard layout and
        #      something like ortholinear ErgoDox.
        #
        #   2. A better keyboard can be placed on top of the laptop, thus randomly
        #      pushing keys of the embedded keyboard.
        #
        if k != embeddedLaptopKeyboardKey: (for device in v.devices: argsAcc.add device)

      # Feature turning on/off flags (arguments list for “xlib-keys-hack”)
      argsAcc.add @[
        "--disable-reset-by-window-focus-event",
        "--default-keyboard-layout=3", # Finnish layout by default

        # Turning on D-Bus IPC
        "--xmobar-indicators",
        "--external-control",
      ]

      argsAcc

  (keyboardsMap, xlibKeysHackArgs)
)

proc isKeyboardPresent(name: string, devices: openArray[string]): bool {.inline.} =
  result = false
  for x in devices:
    if access(x.cstring, R_OK) == 0:
      log.info("Keyboard " & name.escape & " is present")
      result = true
      break

const
  # Options for gaming mode but only for regular qwerty keyboard
  gamingModeRegularKeyboardOptions = ["--real-capslock", "--no-additional-controls"]

  # Ergonomic mode for regular qwerty keyboard when gaming mode is disabled
  nonGamingRegularKeyboardOptions = ["--hold-alt-for-alternative-mode", "--ergonomic-mode"]

  # Regular qwerty keyboard configuration
  regularKeyboardOptions = [
    # Shift “hjkl” on software level.
    "--shift-hjkl",
    # Turn on software debouncer (in order to fix issues with key bouncing on Ducky keyboard).
    "--software-debouncer=90",
    # Right Control as Super to have simmetrical Supers on my laptop’s embedded keyboard
    # (control keys are provided by “additional controls” feature anyway).
    "--right-control-as-super",
  ]

  numericRowShiftOptions = ["--shift-numeric-keys"]

  nonRegularKeyboardOptions = [
    "--real-capslock",
    "--ergonomic-ergodox-mode",
    "--f24-as-vertical-bar",
    "--reset-by-real-escape",
  ]

  disableAdditionalControlsOptions = ["--no-additional-controls"]
  escapeAsAdditionalControlOptions = ["--escape-is-additional-control"]
  disableSuperDoublePressOptions = ["--disable-super-double-press"]
  holdAltForAlternativeModeOptions = ["--hold-alt-for-alternative-mode"]

withStderr:
  log.stage("Requesting ACL access for all evdev input devices")
  callCmd(log, Command(cmd: needexe.grantAccessToInputDevices))

  log.stage("Traversing keyboards and detecting what keyboards are present")

  var
    onlyRegularKeyboardsArePresent: bool = true
    embeddedCoveringKeyboardsArePresent: bool = false
    additionalControlsFeatureIsForced: bool = false
    escapeAsAdditionalControlIsForced: bool = false

  for name, keyboard in pairs(keyboardsMap):
    let isPresent: bool = isKeyboardPresent(name, keyboard.devices)

    if isPresent:
      if keyboard.canBePlacedOnTopOfLaptopKeyboard: embeddedCoveringKeyboardsArePresent = true
      if not keyboard.isRegular: onlyRegularKeyboardsArePresent = false
      if keyboard.forceAdditionalControls: additionalControlsFeatureIsForced = true
      if keyboard.forceEscapeAsAdditionalControl: escapeAsAdditionalControlIsForced = true

  log.info(
    "Only regular keyboards are present: " &
    $onlyRegularKeyboardsArePresent
  )
  log.info(
    "Keyboards that can be placed on top of the laptop keyboard are present: " &
    $embeddedCoveringKeyboardsArePresent
  )
  log.info(
    "Additional controls feature is forced " &
    "(but only if it’s a non-regular keyboard configuration): " &
    $additionalControlsFeatureIsForced
  )
  log.info(
    "Escape as additional control feature is forced: " &
    $escapeAsAdditionalControlIsForced
  )

  log.stage("Enabling conditional options and devices")

  if gamingModeEnabled:
    log.info("Gaming mode enabled")

    if regularKeyboardConfigForced or onlyRegularKeyboardsArePresent:
      log.info("Adding options for gaming mode: " & $gamingModeRegularKeyboardOptions)
      xlibKeysHackArgs.add(gamingModeRegularKeyboardOptions)

    # Gaming-mode specific configuration for non-regular keyboard configuration
    else:
      log.info(
        "Disabling “Super Double Press for Alternative Mode” feature for non-regular " &
        "keyboard configuration: " & $disableSuperDoublePressOptions
      )
      xlibKeysHackArgs.add(disableSuperDoublePressOptions)

  # Only when gaming mode is disabled but regular keyboard configuration is on
  elif regularKeyboardConfigForced or onlyRegularKeyboardsArePresent:
    log.info(
      "Adding non-gaming-mode options for regular keyboard configuration options: " &
      $nonGamingRegularKeyboardOptions
    )
    xlibKeysHackArgs.add(nonGamingRegularKeyboardOptions)

  # Non-gaming mode specific configuration for non-regular keyboard configuration
  else:
    log.info(
      "Enabling “Hold Alt For Alternative Mode” feature for non-regular keyboard configuration: " &
      $holdAltForAlternativeModeOptions
    )
    xlibKeysHackArgs.add(holdAltForAlternativeModeOptions)

  # When a non-regular keyboard is present (which configuration is incompatible
  # with regular embedded laptop keyboard) or a keyboard that can be placed on
  # top of the laptop keyboard then embedded laptop keyboard is disabled.
  if (not embeddedCoveringKeyboardsArePresent) and onlyRegularKeyboardsArePresent:
    const name: string = embeddedLaptopKeyboardKey
    let keyboard: Keyboard = keyboardsMap[name]
    log.info("Enabling " & escape(name) & ": " & $keyboard.devices)
    xlibKeysHackArgs.add(keyboard.devices)

  if regularKeyboardConfigForced or onlyRegularKeyboardsArePresent:
    log.info("Enabling regular keyboard configuration options: " & $regularKeyboardOptions)
    xlibKeysHackArgs.add(regularKeyboardOptions)

    if numericRowShifted:
      log.info(
        "Enabling numeric keys row shifting for regular keyboard configuration: " &
        $numericRowShiftOptions
      )
      xlibKeysHackArgs.add(numericRowShiftOptions)

    if escapeAsAdditionalControlIsForced:
      log.info(
        "Enabling “Escape as Additional Control” feature for regular keyboard configuration: " &
        $escapeAsAdditionalControlOptions
      )
      xlibKeysHackArgs.add(escapeAsAdditionalControlOptions)

  # Non-regular keyboard configuration (e.g. ErgoDox EZ)
  else:
    log.info("Enabling non-regular keyboard configuration options: " & $nonRegularKeyboardOptions)
    xlibKeysHackArgs.add(nonRegularKeyboardOptions)

    if not additionalControlsFeatureIsForced:
      log.info(
        "Disabling “Additional Controls” feature for non-regular keyboard configuration: " &
        $disableAdditionalControlsOptions
      )
      xlibKeysHackArgs.add(disableAdditionalControlsOptions)
    else:
      log.info("Preserving “Additional Controls” feature for non-regular keyboard configuration")

    # Does not make any sense if “Additional Controls” feature is disabled
    if (not additionalControlsFeatureIsForced) and escapeAsAdditionalControlIsForced:
      log.warn(
        "Forcing “Escape as Additional Control” while “Additional Controls” is not forced " &
        "for a non-regular keyboard configuration is NO-OP!"
      )
    elif escapeAsAdditionalControlIsForced:
      log.info(
        "Enabling “Escape as Additional Control” feature for non-regular keyboard configuration: " &
        $escapeAsAdditionalControlOptions
      )
      xlibKeysHackArgs.add(escapeAsAdditionalControlOptions)

  log.ok(needexe.xlibKeysHack.escape & " arguments are now prepared")

  log.stage("Starting threads")
  mainEvents.open()

  type
    OutcomeKind = enum
      xlibKeysHackExited
      xlibKeysHackFailed
      terminationRequest
      terminationThreadFailed
      mainEventLoopFailed

    Outcome {.requiresInit.} = object
      case kind: OutcomeKind
      of xlibKeysHackExited: exitCode: int
      of xlibKeysHackFailed: discard
      of terminationRequest: signal: cint
      of terminationThreadFailed: discard
      of mainEventLoopFailed: discard

  var
    xlibKeysHackThread: Thread[seq[string]]
    xlibKeysHackPid: Option[uint] = uint.none
    terminationSignalWaiterThread: Thread[LogType]
    outcome: Option[ref Outcome] = (ref Outcome).none

  try:
    # Make sure you block the signals before any threads are created.
    # Otherwise the threads won’t inherit the blocking hook and raise
    # SIGINT exception for example to the main thread.
    log.info("Blocking termination signals: " & $terminationSignals)
    blockTerminationSignals()

    log.info("Starting " & escape(needexe.xlibKeysHack) & " thread")
    createThread(xlibKeysHackThread, startXlibKeysHack, xlibKeysHackArgs)

    log.info("Starting termination signal waiter thread")
    createThread(terminationSignalWaiterThread, terminationSignalWaiter, log)

    log.stage("Running main events loop")
    block mainEventLoop:
      while true:
        let event = mainEvents.recv()
        case event.kind

        # xlib-keys-hack thread related
        of started:
          let msg: string = escape(needexe.xlibKeysHack) & " is started (PID: " & $event.pid & ")"
          log.info(msg)
          xlibKeysHackPid = event.pid.some
          notify(log, text = msg)
        of exited:
          let msg: string = escape(needexe.xlibKeysHack) & " is exited with: " & $event.exitCode
          log.warn(msg)
          outcome = (ref Outcome)(kind: xlibKeysHackExited, exitCode: event.exitCode).some
          notify(
            log,
            text = msg,
            urgent = (
              event.exitCode != 0 and
              event.exitCode != SIGTERM and
              event.exitCode != SIGINT
            )
          )
          break mainEventLoop
        of failed:
          let msg: string = escape(needexe.xlibKeysHack) & " is failed: " & event.message
          log.error(msg)
          outcome = (ref Outcome)(kind: xlibKeysHackFailed).some
          notify(log, text = msg, urgent = true)
          break mainEventLoop

        # Termination signal waiter thread related
        of terminationRequest:
          let msg: string = "Termination requested by signal: " & $event.signal
          log.info(msg)
          outcome = (ref Outcome)(kind: terminationRequest, signal: event.signal).some
          notify(log, text = msg)
          break mainEventLoop
        of terminationThreadFailed:
          let msg: string = "Termination signal listener thread failed: " & event.errMsg
          log.error(msg)
          outcome = (ref Outcome)(kind: terminationThreadFailed).some
          notify(log, text = msg, urgent = true)
          break mainEventLoop

  except CatchableError as error:
    log.error("Main event loop exception: " & error.msg)
    outcome = (ref Outcome)(kind: mainEventLoopFailed).some

  finally:
    log.stage("Cleaning up")

    template threadCleanup[T](
      pid: Pid,
      signal: cint,
      thread: Thread[T],
      threadName: string,
    ): void =
      block:
        if kill(pid, signal) != 0:
          let error: cint = errno
          case posix.errno
          of posix.ESRCH: discard # Not alive, considering a success
          else: raiseOSError(OSErrorCode(error))
        log.debug("Waiting for " & escape(threadName) & " thread to finish")
        if waitForCondition(not running(thread)):
          log.debug("Joining " & escape(threadName) & " thread")
          joinThread(thread)
          log.ok("Successfully cleaned up " & escape(threadName) & " thread")
        else:
          log.error("Failed to cleanup " & escape(threadName) & " thread (ignoring it)")

    # xlib-keys-hack thread cleanup
    if xlibKeysHackPid.isSome:
      log.debug("Sending SIGTERM to xlib-keys-hack PID " & $xlibKeysHackPid.get)
      threadCleanup(xlibKeysHackPid.get.Pid, SIGTERM, xlibKeysHackThread, "xlib-keys-hack")
    else:
      log.skip(escape(needexe.xlibKeysHack) & " PID was not registered")

    # Termination signal waiter thread cleanup
    log.debug("Triggering SIGUSR1 to make sure termination signal waiter thread is finishing")
    threadCleanup(pid, SIGUSR1, terminationSignalWaiterThread, "termination signal waiter")

    log.debug("Triggering release events for all currently pressed keys")
    cleanupPressedKeys(log)

    log.debug("Closing the main events communication channel")
    mainEvents.close()

  if outcome.isNone: fail "Unexpectedly Outcome was not set"
  case outcome.get.kind
  of xlibKeysHackExited:
    let exitCode: int = outcome.get.exitCode
    log.info("Exiting with reported xlib-keys-hack exit code: " & $exitCode)
    exitCode.quit
  of xlibKeysHackFailed:
    fail "xlib-keys-hack thread failed"
  of terminationRequest:
    # Conventional shell exit status: 128 + signal number.
    let finalSignal: int = 128 + outcome.get.signal
    log.ok("Received signal " & $outcome.get.signal & ", exiting with " & $finalSignal)
    finalSignal.quit
  of terminationThreadFailed:
    fail "Termination signal waitier thread failed"
  of mainEventLoopFailed:
    fail "Main event loop failed"
