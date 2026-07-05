# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# A little logging boilerplate.

from std/times import nil

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

    timeFormat*: string = "HH:mm:ss'.'fff"

    writeLine*: proc (line: string): void {.inline.} =
      proc (line: string): void {.inline.} = stderr.writeLine(line)

# Generic log function to construct the other log functions of
template log*(x: Log, msg: string): void =
  x.writeLine("[" & times.format(times.now(), x.timeFormat) & "]" & msg)

template fail*(x: Log, msg: string): void = (if x.showFail: x.log("[FAIL] " & msg))
template error*(x: Log, msg: string): void = (if x.showError: x.log("[ERROR] " & msg))
template warn*(x: Log, msg: string): void = (if x.showWarn: x.log("[WARNING] " & msg))
template stage*(x: Log, msg: string): void = (if x.showStage: x.log("[STAGE] " & msg))
template info*(x: Log, msg: string): void = (if x.showInfo: x.log("[INFO] " & msg))
template ok*(x: Log, msg: string): void = (if x.showOk: x.log("[OK] " & msg))
template skip*(x: Log, msg: string): void = (if x.showSkip: x.log("[SKIP] " & msg))
template debug*(x: Log, msg: string): void = (if x.showDebug: x.log("[DEBUG] " & msg))

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
