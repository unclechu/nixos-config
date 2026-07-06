# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# A little logging boilerplate.

from std/times import nil

const defaultTimeFormat*: string = "HH:mm:ss'.'fff"

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

  LogWriter* = proc(line: string) {.nimcall, gcsafe, inline.}

  # TODO: Try when nimlsp works with Nim 2.2.*
  #
  # There seem to be some compiler bugs in Nim 2.0.*
  # > Error: unhandled exception: types.nim(1850, 11) `ty.kind in {tyTuple, tyObject}`  [AssertionDefect]
  #
  # Log*[TimeFormat: static string = defaultTimeFormat] = object
  # template log*[TimeFormat: static string](x: lent Log[TimeFormat], msg: string): void =
  # DefaultLog* = Log[defaultTimeFormat]
  # x.writeLine("[" & times.format(times.now(), TimeFormat) & "]" & msg)
  #
  Log* = object
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

    writeLine*: LogWriter =
      proc (line: string): void {.nimcall, gcsafe, inline.} =
        stderr.writeLine(line)

  DefaultLog* = Log

# Generic log function to construct the other log functions of
template log*(x: lent Log, msg: string): void =
  x.writeLine("[" & times.format(times.now(), defaultTimeFormat) & "]" & msg)

template fail*(x: lent Log, msg: string): void =
  if x.showFail: x.log("[FAIL] " & msg)

template error*(x: lent Log, msg: string): void =
  if x.showError: x.log("[ERROR] " & msg)

template warn*(x: lent Log, msg: string): void =
  if x.showWarn: x.log("[WARNING] " & msg)

template stage*(x: lent Log, msg: string): void =
  if x.showStage: x.log("[STAGE] " & msg)

template info*(x: lent Log, msg: string): void =
  if x.showInfo: x.log("[INFO] " & msg)

template ok*(x: lent Log, msg: string): void =
  if x.showOk: x.log("[OK] " & msg)

template skip*(x: lent Log, msg: string): void =
  if x.showSkip: x.log("[SKIP] " & msg)

template debug*(x: lent Log, msg: string): void =
  if x.showDebug: x.log("[DEBUG] " & msg)

# Set maximum noisiness level for the log.
#
# Note that it will override your individual `show*` flags configuration.
proc setNoisinessLevel*(x: var Log, maxLogLevel: LogNoisinessLevel): void {.inline.} =
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
proc silence*(x: var Log): void {.inline.} = x.setNoisinessLevel(silenceLevel)
