# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

from os import nil
from sequtils import nil
from strutils import nil

# Runtime executables checking

const env*: string = "env"
const setsid*: string = "setsid"
const pgrep*: string = "pgrep"
const xset*: string = "xset"
const setxkbmap*: string = "setxkbmap"
const numlockx*: string = "numlockx"
const notifySend*: string = "notify-send"
const xbindkeys*: string = "xbindkeys"
const wenzelsXlibKeysHack*: string = "wenzels-xlib-keys-hack"

# Not adding to `knownExecutables` by design
# (Nix should not add them as dependencies directly).
const wenzelsKeyboard*: string = "wenzels-keyboard"
const xlibKeysHack*: string = "xlib-keys-hack"
const xlibKeysHackWatchForWindowFocusEvents*: string = "xlib-keys-hack-watch-for-window-focus-events"

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

const artifactExecutables*: array[4, string] = [
  xbindkeys,
  wenzelsXlibKeysHack,
  xlibKeysHack,
  xlibKeysHackWatchForWindowFocusEvents,
]

# Mark as {.used.} because runtime dependencies checking is removed for Nix derivation
proc needExe(executableName: string): void {.used.} =
  if not (executableName in knownExecutables):
    quit(
      "Unknown executable: " & strutils.escape(executableName) &
      " (must be one of: " & $knownExecutables & ")",
      1
    )
  elif os.findExe(executableName) == "":
    quit("Missing executable dependency: " & strutils.escape(executableName), 1)
  else:
    sequtils.keepItIf(uncheckedExecutables, it != executableName)

template checkExecutableDependencies*(): void =
  block:
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
    if uncheckedExecutables.len > 0:
      quit("Some executables left unchecked: " & $uncheckedExecutables, 1)

template assertExecutableNames(executableNames: untyped): void =
  for name in executableNames: doAssert strutils.allCharsInSet(name, {'a'..'z', '-'})
# Assert executable name restrictions in compile-time
# (so that they can be safely interpolated into `pgrep` regexes).
static:
  assertExecutableNames(knownExecutables)
  assertExecutableNames(artifactExecutables)
