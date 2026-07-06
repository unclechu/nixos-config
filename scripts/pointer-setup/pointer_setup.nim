# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# Generic xinput pointer setup script.

from std/locks import nil
from std/options import nil
from std/os import nil
from std/osproc import nil
from std/re import nil
from std/sequtils import nil
from std/sets import nil
from std/streams import nil
from std/strutils import nil
from std/tables import `[]=`, `[]`

from cliargs import nil
from either import Either, left, right
from log as logging import fail, debug, skip, ok, warn

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

# Standard error handling

# To make sure there are no stderr clashes when multiple processes/threads are writing to it
var stderrLock: locks.Lock

# Write to stderr clash-free
proc writeStderr(line: string): void {.inline.} =
  locks.withLock stderrLock: (stderr.writeLine(line); stderr.flushFile())

template withStderr(body: untyped) =
  (locks.initLock(stderrLock); try: body finally: locks.deinitLock(stderrLock))

let log: logging.DefaultLog = logging.Log(writeLine: writeStderr)

# Fail the program with a message
template fail(msg: string, exitCode: int = 1): void = (log.fail(msg); quit exitCode)

# Executable name constants

const xinput: string = "xinput"
const notifySend: string = "notify-send"

const knownExecutables: array[2, string] = [
  xinput,
  notifySend,
]

var uncheckedExecutables: seq[string] = @knownExecutables

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
template allKnownExecutablesAreChecked(): void {.used.} =
  if uncheckedExecutables.len > 0:
    fail("Some executables left unchecked: " & $uncheckedExecutables)

# Redirect stderr of a subprocess to the main process stderr, line-by-line, clash-free.
proc forwardStderr(p: osproc.Process): void {.thread.} =
  let source = osproc.errorStream(p)
  var line: string
  while streams.readLine(source, line): writeStderr(line)

# Subprocess helpers

type SubProc = object

# Open a new process with stdin and stdout streams
proc startInOutInteraction(_: typedesc[SubProc], command: Command): InOutProc =
  log.debug("startInOutInteraction: " & $command)
  let p = osproc.startProcess(command.cmd, args = command.args, options = {osproc.poUsePath})
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
template spawn(_: typedesc[SubProc], command: Command): void =
  block:
    log.debug("spawn: " & $command)
    discard osproc.startProcess(
      command.cmd,
      args = command.args,
      options = {osproc.poUsePath, osproc.poDaemon, osproc.poParentStreams}
    )

# Start a sub-process in background inheriting stdin, stdout, and stderr and wait for success.
template callCmd(_: typedesc[SubProc], command: Command): void =
  block:
    log.debug("callCmd: " & $command)
    let p: osproc.Process = osproc.startProcess(
      command.cmd,
      args = command.args,
      options = {osproc.poUsePath, osproc.poParentStreams}
    )
    doAssert (osproc.waitForExit(p) == 0)
    osproc.close(p)

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

# Command-line-arguments

const
  pointerNameOpt: string = "pointer-name"
  deviceNameOpt: string = "device-name"
  deviceProductIdOpt: string = "device-product-id"
  setPropOpt: string = "set-prop"

let argOptsSpec: cliargs.ArgOptions = cliargs.makeOptionsSpec({
  pointerNameOpt: cliargs.makeStrOption(
    optRequired = true,
    optInfo =
      "Human-readable pointer name for the desktop notifications.\n\n" &
      "Example: " & strutils.escape("logitech-wireless-ambidextrous-small-mouse"),
  ),
  deviceNameOpt: cliargs.makeStrOption(
    optRequired = true,
    optInfo =
      "xinput device name filter for the pointer.\n\n" &
      "Example: --" & deviceNameOpt & "=" &
      strutils.escape("Logitech Wireless Mouse"),
  ),
  deviceProductIdOpt: cliargs.makeStrOption(
    optInfo =
      "Additional xinput pointer filter that compares against " &
      "the " & strutils.escape("Device Product ID") & " xinput property.\n\n" &
      "Example: --" & deviceProductIdOpt & "=" & strutils.escape("1133, 16469"),
  ),
  setPropOpt: cliargs.makeStrOption(
    optPlaceholder = strutils.escape("NAME=VAL"),
    optInfo =
      "Set xinput property value for the pointer. " &
      "Takes property name and the value to set that property to, " &
      "divided with an equal sign. " &
      "If the property takes multiple values, separate them with a space " &
      "(they will be passed to xinput as multiple arguments).\n\n" &
      "Example: --" & setPropOpt & "=" &
      strutils.escape("libinput Scroll Method Enabled=0 0 1"),
  ),
  "help": cliargs.makeFlagOption(
    optShort = 'h',
    optInfo = "Show this usage information.",
  ),
})

type
  SetProps[T] = tables.Table[T, seq[string]]
  SetProp = tuple[name: string, values: seq[string]]

const setPropSep = "="

template parseSetPropValue(value: string): Either[string, SetProp] =
  block:
    let kvSplit: seq[string] = strutils.split(value, setPropSep, maxsplit = 1)
    if kvSplit.len != 2:
      left[string, SetProp](
        "Key-value split must have 2 parts, got: " & $kvSplit.len &
        " (missing " & strutils.escape(setPropSep) & " separator?)"
      )
    elif kvSplit[0].len < 1: left[string, SetProp]("Key is empty")
    elif kvSplit[1].len < 1: left[string, SetProp]("Value is empty")
    else:
      let valSplit: seq[string] = strutils.split(kvSplit[1], " ")
      if valSplit.len < 1: left[string, SetProp]("Value split is empty")
      else: right[string, SetProp]((name: kvSplit[0], values: valSplit))

const productIdPropName: string = "Device Product ID";

# The main program

withStderr:
  # Guard dependencies (this piece is parsed by Nix, the shape is important to match the regex)
  needExe("xinput")
  needExe("notify-send")
  allKnownExecutablesAreChecked()

  let args: cliargs.ParsedArgs = cliargs.parseArgs(
    argOptsSpec,
    usageDescription =
      "A utility application that applies settings for an xinput pointer using filters to find " &
      "the right pointer to apply the settings for.",
    usageLineLenLimit = 90,
    usageInfoTransform = proc (usage: string, errors: string): string =
      (if errors.len > 0: "\n" & errors & "\n" else: "") & "\n" & usage & "\n",
    errWriteLine = proc (line: string): void = writeStderr(line),
    validate = proc (args: cliargs.ParsedArgs, _: string): string =
      var errors: seq[string] = @[]

      if args.positional.len > 0: errors.add(
        "This application does not accept any positional arguments!\n" &
        "Got: " & $args.positional
      )

      block:
        for val in cliargs.getOptionValue(args, setPropOpt).strValuesAcc:
          let setPropValue: Either[string, SetProp] = parseSetPropValue(val)
          if not setPropValue.isRight: errors.add(
            "Incorrect --" & setPropOpt & " value: " & strutils.escape(val) & ".\n" &
            "Parsing failed with: " & setPropValue.left
          )

      if errors.len <= 0: "" else: strutils.join(errors, "\n\n")
  )

  # Parsed arguments input
  let
    pointerName: string =
      options.get(cliargs.getOptionValue(args, pointerNameOpt).strValue)
    deviceName: string =
      options.get(cliargs.getOptionValue(args, deviceNameOpt).strValue)
    deviceProductId: options.Option[string] =
      cliargs.getOptionValue(args, deviceProductIdOpt).strValue
    setProps: SetProps[string] = block:
      var acc: SetProps[string] = tables.initTable[string, seq[string]]()
      for val in cliargs.getOptionValue(args, setPropOpt).strValuesAcc:
        let x: SetProp = parseSetPropValue(val).right
        acc[x.name] = x.values
      acc

  # xinput output parsing patterns
  let
    xinputDeviceLinePattern: re.Regex =
      re.re("↳ " & re.escapeRe(deviceName) & r"\s+id=\d+\s+")

    xinputDeviceIdPattern: re.Regex = re.re(r"id=(\d+)")

    xinputPropPattern: re.Regex =
      re.re(
        r"^\s*([^:]+?)" & # Property name
        r"\s+\((\d+)\):" & # Property ID
        r"\s*([^:]+?)\s*$" # Property value
      )

  let xinputDevicesListProc: InOutProc = block:
    log.debug("Requesting a list of devices by calling xinput")
    let p: InOutProc =
      SubProc.startInOutInteraction(Command(cmd: xinput, args: @["list", "--short"]))
    streams.close(p.stdinStream) # xinput should not read anything from standard input
    p

  var
    deviceFound: bool = false
    deviceLine: string

  while streams.readLine(xinputDevicesListProc.stdoutStream, deviceLine):
    if not re.contains(deviceLine, xinputDeviceLinePattern): continue

    let deviceId: uint = block:
      var captures: array[1, string]
      if re.find(deviceLine, xinputDeviceIdPattern, captures) < 0:
        fail("Failed to extract xinput device ID from device line: " & strutils.escape(deviceLine))
      let id: uint = strutils.parseInt(captures[0]).uint
      log.debug(
        "Found device by device name " & strutils.escape(deviceName) &
        " (device ID: " & $id & ")"
      )
      id

    let xinputDevicePropsListProc: InOutProc = block:
      log.debug(
        "Reading xinput properties for device " &
        strutils.escape(deviceName) & " ID " & $deviceId
      )
      let p: InOutProc =
        SubProc.startInOutInteraction(Command(cmd: xinput, args: @["list-props", $deviceId]))
      streams.close(p.stdinStream) # xinput should not read anything from standard input
      p

    var
      propsToSet: SetProps[uint] = tables.initTable[uint, seq[string]]()
      foundPropNames: sets.HashSet[string] = sets.initHashSet[string]()
      deviceMatched: bool = if options.isSome(deviceProductId): false else: true
      foundDeviceProductIdProp: bool = false
      propLine: string

    while streams.readLine(xinputDevicePropsListProc.stdoutStream, propLine):
      var propCapture: array[3, string]
      if not re.match(propLine, xinputPropPattern, propCapture): continue

      let
        propName: string = propCapture[0]
        propId: uint = strutils.parseInt(propCapture[1]).uint
        propValue: string = propCapture[2]

      if propName == productIdPropName and options.isSome(deviceProductId):
        foundDeviceProductIdProp = true
        let expectedVal: string = options.get(deviceProductId)
        if expectedVal == propValue:
          deviceMatched = true
          log.ok(
            "xinput device " & strutils.escape(deviceName) & " ID " & $deviceId &
            " matched " & strutils.escape(productIdPropName) & " property value " &
            strutils.escape(expectedVal)
          )
        else:
          log.skip(
            "xinput device " & strutils.escape(deviceName) & " ID " & $deviceId &
            " mismatched expected " & strutils.escape(productIdPropName) & " property value " &
            strutils.escape(expectedVal) & ", got: " & strutils.escape(propValue)
          )
          break # Skipping the device, do not need to read any more props

      for name, values in tables.pairs(setProps):
        if name != propName: continue
        log.debug(
          "Found property ID " & $propId & " by property name " & strutils.escape(propName) &
          " for xinput device " & strutils.escape(deviceName) & " ID " & $deviceId
        )
        sets.incl(foundPropNames, propName)
        propsToSet[propId] = values

    if (not foundDeviceProductIdProp) and options.isSome(deviceProductId):
      log.warn(
        "xinput property " & strutils.escape(productIdPropName) &
        " was not found for xinput device" & strutils.escape(deviceName) & " ID " & $deviceId
      )

    if deviceMatched:
      # Validating found props with ones that are requested to be set:
      var expectedPropNames = sets.initHashSet[string]()
      for name in tables.keys(setProps): sets.incl(expectedPropNames, name)
      if foundPropNames != expectedPropNames:
        fail(
          "Some of the xinput device properties were not found for xinput device " &
          strutils.escape(deviceName) & " ID " & $deviceId & " expected " &
          strutils.escape($expectedPropNames) & " but got only: " & strutils.escape($foundPropNames)
        )
      for propId, propValues in tables.pairs(propsToSet):
        log.debug(
          "Setting values " & $propValues & " for property ID " & $propId & " for xinput device " &
          strutils.escape(deviceName) & " ID " & $deviceId
        )
        var setPropArgs: seq[string] = @["set-prop", $deviceId, $propId]
        setPropArgs.add(propValues)
        SubProc.callCmd(Command(cmd: xinput, args: setPropArgs))
      log.ok(
        "xinput device " & strutils.escape(deviceName) & " ID " & $deviceId &
        " was successfully configured"
      )

    deviceFound = deviceMatched

  log.debug("Closing input devices list process handle")
  xinputDevicesListProc.close()

  if deviceFound:
    log.ok("Pointer " & strutils.escape(pointerName) & " was successfully configured")
    DesktopNotification.notify(pointerName, "Pointer successfully configured")
  else:
    fail(
      "Pointer " & strutils.escape(pointerName) & " is not found " &
      "by " & strutils.escape(deviceName) & " device name" & (
        if options.isNone(deviceProductId): "" else:
        " and by " & strutils.escape(options.get(deviceProductId)) &
        " value for " & strutils.escape(productIdPropName) & " property"
      )
    )
