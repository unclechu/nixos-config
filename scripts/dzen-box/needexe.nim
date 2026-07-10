# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

from os import findExe
from sequtils import keepItIf
from strutils import escape

# Runtime executables checking

const
  env*: string = "env"
  setsid*: string = "setsid"
  dzen2*: string = "dzen2"

const knownExecutables = [
  env,
  setsid,
  dzen2,
]

var uncheckedExecutables: seq[string] = @knownExecutables

proc needExe(executableName: string, lenient: bool = false): void {.used.} =
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
    needExe("env")
    needExe("setsid")
    needExe("dzen2")
    if uncheckedExecutables.len > 0:
      quit("Some executables left unchecked: " & $uncheckedExecutables, 1)
