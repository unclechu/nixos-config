# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

from strutils import parseInt, format
from re import Regex, re, find
from options import Option, isSome, isNone, some, none, get
from streams import Stream, readLine, writeLine
from locks import Lock, initLock, acquire, release
from osproc import outputStream, errorStream, waitForExit, close, terminate

from ipc import nil
from types import State
from needexe import xdotool, xwininfo

type
  Filter = object
    class: Option[string]
    name:  Option[string]
  AppDecl = tuple[name: string, filters: seq[Filter]]
  AppMapping = array[2, AppDecl]
  # AppMapping = array[9, AppDecl]

let
  nop = string.none
  mapping*: AppMapping =
    [ #("audacious",   @[Filter(class: "^Audacious$".some,   name: nop)])
    # , ("thunderbird", @[Filter(class: "^Thunderbird$".some, name: nop)])
    # , ("gajim",       @[Filter(class: "^Gajim$".some,       name: nop)])
      ("nheko",       @[Filter(class: "^nheko$".some,       name: nop)])
    # , ("keepassx",    @[Filter(class: "^Keepassx$".some,    name: nop)])
    , ("qbittorrent", @[Filter(class: "^qBittorrent$".some, name: nop)])
    # , ("hexchat",     @[Filter(class: "^Hexchat$".some,     name: nop)])
    # , ("doublecmd",   @[Filter(class: "^Doublecmd$".some,   name: nop)])
    # , ("gmrun",       @[Filter(class: "^Gmrun$".some,       name: nop)])
    ]

var
  appsCache: Option[seq[string]] = seq[string].none
  L: Lock

proc getApps*(): seq[string] =
  if appsCache.isSome: return appsCache.get
  result = @[]
  for x in mapping: result.add x.name
  appsCache = result.some

proc childProc( cmd: string; args: openArray[string]
              ; handler: proc (hproc: osproc.Process; sout: Stream)
              ; careAboutFail: Option[string] ) =

  var
    hproc: osproc.Process
    sout:  Stream
    serr:  Stream
  try: # FIXME this is hacky
    hproc = osproc.startProcess(command=cmd, args=args, options={osproc.poUsePath})
    sout  = hproc.outputStream
    serr  = hproc.errorStream
  except OSError:
    L.acquire
    stderr.writeLine "Gotcha OSError [1]: " & getCurrentExceptionMsg()
    L.release

    childProc(cmd, args, handler, careAboutFail)
    return

  handler(hproc, sout)

  try: # FIXME this is hacky
    let code: int = hproc.waitForExit

    if careAboutFail.isSome and code != 0:
      var line: string = ""
      L.acquire
      while serr.readLine(line): stderr.writeLine line
      quit(careAboutFail.get & " failed with exit code: " & $code, 1)

    hproc.close
  except OSError:
    L.acquire
    stderr.writeLine "Gotcha OSError [2]: " & getCurrentExceptionMsg()
    L.release

    childProc(cmd, args, handler, careAboutFail)
    return

proc getRootWnd(): uint32 =

  var matches: array[1, string] = [""]

  proc handler(hproc: osproc.Process; sout: Stream) =
    var line: string = ""
    while sout.readLine(line):
      if line == "":
        continue
      elif line.find(re"Window\sid:\s+(\d+)", matches) != -1:
        hproc.terminate
        break
    if matches[0] == "":
      L.acquire; quit("Root window id not found!", 1)

  childProc(
    xwininfo, ["-int", "-root"], handler, "Getting root window id".some
  )
  matches[0].parseInt.uint32

proc getParentWnd(childWnd: uint32): Option[uint32] =

  var matches: array[1, string] = [""]

  proc handler(hproc: osproc.Process; sout: Stream) =
    var line: string = ""
    while sout.readLine(line):
      if line == "":
        continue
      elif line.find(re"Parent\swindow\sid:\s+(\d+)", matches) != -1:
        hproc.terminate
        break
    if matches[0] == "":
      L.acquire
      quit("Parent window id for '$1' not found!".format(childWnd), 1)

  childProc(
    xwininfo, ["-int", "-children", "-id", $childWnd], handler, string.none
  )

  if matches[0] == "": uint32.none
  else: matches[0].parseInt.uint32.some

var rootWnd: uint32

proc handleWnd(wnd: uint32; state: State) =
  let parwnd: Option[uint32] = wnd.getParentWnd
  if parwnd.isNone or parwnd.get == rootWnd: return
  {.gcsafe.}:
    if state == toggle:
      # TODO do this hacky stuff different way
      ipc.setState(parwnd, State.off, failProtect=true)
      ipc.setState(parwnd, State.on,  failProtect=true)
    else:
      ipc.setState(parwnd, state, failProtect=true)

proc handleAppFilter(filter: Filter; state: State) =
  var args: seq[string] = @["search", "--onlyvisible", "--all"]
  if filter.class.isSome: args.add(["--class", filter.class.get])
  if filter.name.isSome: args.add(["--name", filter.name.get])

  proc handler(hproc: osproc.Process; sout: Stream) =
    var line: string = ""
    while sout.readLine(line): handleWnd(line.parseInt.uint32, state)

  childProc(xdotool, args, handler, string.none)

proc handleApp(idx: int, state: State) =
  let app: AppDecl = mapping[idx]
  L.acquire; "Handling '$1' application…".format(app.name).echo; L.release
  for n, x in app.filters.pairs: handleAppFilter(x, state)

proc handleApps*(indexes: seq[int]; state: State) =
  rootWnd = getRootWnd()
  for n, idx in indexes.pairs: handleApp(idx, state)
  L.acquire; "Done with apps.".echo; L.release

initLock L
