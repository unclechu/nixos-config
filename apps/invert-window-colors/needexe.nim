# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

from os import nil
from sequtils import nil
from strutils import nil

# Runtime executables checking

const xdotool*: string = "xdotool"
const xwininfo*: string = "xwininfo"

const knownExecutables: array[2, string] = [
  xdotool,
  xwininfo,
]

var uncheckedExecutables: seq[string] = @knownExecutables

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
    needExe("xdotool")
    needExe("xwininfo")
    if uncheckedExecutables.len > 0:
      quit("Some executables left unchecked: " & $uncheckedExecutables, 1)
