# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# A little logging boilerplate.

from std/times import nil

const defaultTimeFormat*: string = "HH:mm:ss'.'fff"

type StderrWriter* = object

template writeLogLine*(writer: StderrWriter, line: string): void =
  stderr.writeLine(line)

type
  LogNoisinessLevel* = enum
    silenceLevel # Log is silent with this level
    failLevel
    errorLevel
    warnLevel
    stageLevel
    infoLevel
    okLevel
    skipLevel
    debugLevel

  Log*[
    TimeFormat: static string = defaultTimeFormat,
    Writer = StderrWriter,
  ] = object
    # Booleans are cheaper to check on each log write than for example checking something like
    # `set[LogNoisinessLevel]`.
    showFail*: bool = true
    showError*: bool = true
    showWarn*: bool = true
    showStage*: bool = true
    showInfo*: bool = true
    showOk*: bool = true
    showSkip*: bool = true
    showDebug*: bool = true
    writer*: Writer

# Generic log function to construct the other log functions of
template log*[TimeFormat: static string, Writer](
  x: lent Log[TimeFormat, Writer],
  msg: string,
): void =
  x.writer.writeLogLine("[" & times.format(times.now(), TimeFormat) & "]" & msg)

template fail*[TimeFormat: static string, Writer](
  x: lent Log[TimeFormat, Writer],
  msg: string,
): void =
  if x.showFail: x.log("[FAIL] " & msg)

template error*[TimeFormat: static string, Writer](
  x: lent Log[TimeFormat, Writer],
  msg: string,
): void =
  if x.showError: x.log("[ERROR] " & msg)

template warn*[TimeFormat: static string, Writer](
  x: lent Log[TimeFormat, Writer],
  msg: string
): void =
  if x.showWarn: x.log("[WARNING] " & msg)

template stage*[TimeFormat: static string, Writer](
  x: lent Log[TimeFormat, Writer],
  msg: string
): void =
  if x.showStage: x.log("[STAGE] " & msg)

template info*[TimeFormat: static string, Writer](
  x: lent Log[TimeFormat, Writer],
  msg: string
): void =
  if x.showInfo: x.log("[INFO] " & msg)

template ok*[TimeFormat: static string, Writer](
  x: lent Log[TimeFormat, Writer],
  msg: string
): void =
  if x.showOk: x.log("[OK] " & msg)

template skip*[TimeFormat: static string, Writer](
  x: lent Log[TimeFormat, Writer],
  msg: string
): void =
  if x.showSkip: x.log("[SKIP] " & msg)

template debug*[TimeFormat: static string, Writer](
  x: lent Log[TimeFormat, Writer],
  msg: string
): void =
  if x.showDebug: x.log("[DEBUG] " & msg)

# Set maximum noisiness level for the log.
#
# Note that it will override your individual `show*` flags configuration.
proc setNoisinessLevel*[TimeFormat: static string, Writer](
  x: var Log[TimeFormat, Writer],
  maxLogLevel: LogNoisinessLevel,
): void {.inline.} =
  x.showFail = maxLogLevel >= failLevel
  x.showError = maxLogLevel >= errorLevel
  x.showWarn = maxLogLevel >= warnLevel
  x.showStage = maxLogLevel >= stageLevel
  x.showInfo = maxLogLevel >= infoLevel
  x.showOk = maxLogLevel >= okLevel
  x.showSkip = maxLogLevel >= skipLevel
  x.showDebug = maxLogLevel >= debugLevel

# Make the log absolutely silent.
#
# Note that it will override your individual `show*` flags configuration.
proc silence*[TimeFormat: static string, Writer](
  x: var Log[TimeFormat, Writer],
): void {.inline.} =
  x.setNoisinessLevel(silenceLevel)
