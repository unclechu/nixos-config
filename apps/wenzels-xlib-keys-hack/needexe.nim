# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

from os import findExe
from sequtils import keepItIf
from strutils import escape

# Runtime executables checking

const
  xlibKeysHack*: string = "xlib-keys-hack"
  env*: string = "env"
  setsid*: string = "setsid"
  dash*: string = "dash"
  xinput*: string = "xinput"
  sed*: string = "sed"
  xargs*: string = "xargs"
  sort*: string = "sort"
  notifySend*: string = "notify-send"
  xdotool*: string = "xdotool"

const
  # Not adding to `knownExecutables` by design
  # (Nix should not add it as a dependency directly, since it requires root sticky bit).
  grantAccessToInputDevices*: string = "grant-access-to-input-devices"

const knownExecutables: array[10, string] = [
  xlibKeysHack,
  env,
  setsid,
  dash,
  xinput,
  sed,
  xargs,
  sort,
  notifySend,
  xdotool,
]

var uncheckedExecutables: seq[string] = @knownExecutables

proc needExe(executableName: string, lenient: bool = false): void =
  if (not lenient) and (not (executableName in knownExecutables)):
    quit(
      "Unknown executable: " & executableName.escape &
      " (must be one of: " & $knownExecutables & ")",
      1
    )
  elif executableName.findExe == "":
    quit("Missing executable dependency: " & executableName.escape, 1)
  elif not lenient:
    keepItIf(uncheckedExecutables, it != executableName)

template checkExecutableDependencies*(): void =
  block:
    # Guard dependencies (this piece is parsed by Nix, the shape is important to match the regex)
    needExe("xlib-keys-hack")
    needExe("env")
    needExe("setsid")
    needExe("dash")
    needExe("xinput")
    needExe("sed")
    needExe("xargs")
    needExe("sort")
    needExe("notify-send")
    needExe("xdotool")
    if uncheckedExecutables.len > 0:
      quit("Some executables left unchecked: " & $uncheckedExecutables, 1)

    # Guard runtime-only dependencies
    needExe(grantAccessToInputDevices, lenient = true)
