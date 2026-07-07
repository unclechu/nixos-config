# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

from strutils import format, join, parseUInt, replace, split
from sequtils import filterIt, mapIt, newSeqWith, keepItIf
from re import re, match
from os import paramStr, paramCount, commandLineParams
from options import Option, isSome, isNone, some, none, get

from dbus import asDbusValue, close

from needexe import checkExecutableDependencies
from types import State
from ipc import dbusReq, setState
from app import getApps, handleApps

checkExecutableDependencies()

const
  stCmd     = [ "on",     "off",     "toggle"     ]
  stCmdEnum = [ State.on, State.off, State.toggle ]

proc isForCommand(): bool {.inline.} = (
  paramCount() in [2, 3] and paramStr(1) == "for" and
  paramStr(2).match(re"\d+") and
  (paramCount() == 2 or paramStr(3) in stCmd)
  )

proc isAppCommand(): bool {.inline.} = (
  paramCount() in [2, 3] and paramStr(1) == "app" and
  (paramStr(2) == "all" or paramStr(2).match(re"[a-z, ]+")) and
  (paramCount() == 2 or paramStr(3) in stCmd)
  )

proc toState(cmd: string): State {.inline.} = stCmdEnum[stCmd.find cmd]

proc usageInfo(): string = """
$1 usage info:
  `$1 help` to show this usage info
  `$1` without any arguments will toggle current focused window
  `$1 toggle` explicitly toggle
  `$1 on`
  `$1 off`
  `$1 for 123456` will toggle window where "123456" is X window id
    (usually it's wrapper-window, the compton needs it)
  `$1 for 123456 toggle` explicitly toggle
  `$1 for 123456 on`
  `$1 for 123456 off`
  `$1 app all` to toggle all known apps
  `$1 app all toggle` explicitly toggle
  `$1 app all on`
  `$1 app all off`
  `$1 app gajim` toggle windows of gajim apps
    (mapping for "gajim" must be declared in this app code)
  `$1 app gajim toggle` explicitly toggle
  `$1 app gajim on`
  `$1 app gajim off`
  `$1 app gajim,audacious`
  `$1 app gajim,audacious toggle`
  `$1 app 'gajim, audacious' on`
  `$1 app 'gajim , audacious' off`
""".format(paramStr 0)

dbusReq("opts_set", "track_focus".asDbusValue, true.asDbusValue).close

if paramCount() == 0 or (paramCount() == 1 and paramStr(1) in stCmd):
  let s = if paramCount() == 0: toggle else: toState(paramStr 1)
  setState(uint32.none, s)
elif isForCommand():
  let s = if paramCount() == 2: toggle else: toState(paramStr 3)
  setState(paramStr(2).parseUInt.uint32.some, s)

elif isAppCommand():

  let s = if paramCount() == 2: toggle else: toState(paramStr 3)
  var appsIndexes: seq[int]

  if paramStr(2) == "all":
    appsIndexes = getApps().len.newSeqWith(-1)
    for idx, _ in getApps().pairs: appsIndexes[idx] = idx
  else:

    let apps: seq[string] =
      paramStr(2).replace(" ", "").split(',').filterIt(it != "")

    for app in apps:
      if app notin getApps():
        quit("Unknown app: '$1'".format(app), 1)

    appsIndexes = apps.len.newSeqWith(-1)
    for n, app in apps.pairs: appsIndexes[n] = getApps().find app

  handleApps(appsIndexes, s)

elif paramCount() == 1 and paramStr(1) == "help":
  stdout.write usageInfo()
  quit 2
else:
  stderr.write usageInfo()
  stderr.writeLine(
    "Incorrect arguments: [$1]"
      .format(commandLineParams().mapIt("'" & it & "'").join ", "))
  quit 1
