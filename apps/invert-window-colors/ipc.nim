# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

from os import getenv
from locks import Lock, initLock, acquire, release
from options import Option, isSome, isNone, some, none, get

from dbus import Bus, ObjectPath, Message, DbusValue, Reply, DbusRemoteException
from types import State

proc toDbusDisplayName(display: string): string =
  result = newStringOfCap(display.len)
  for ch in display:
    if (ch >= 'a' and ch <= 'z') or
       (ch >= 'A' and ch <= 'Z') or
       (ch >= '0' and ch <= '9'):
      result.add ch
    else:
      result.add '_'

let
  bus: Bus        = dbus.getBus dbus.DBUS_BUS_SESSION
  dpy: string     = getEnv("DISPLAY").toDbusDisplayName
  dst: string     = "com.github.chjj.compton." & dpy
  obj: ObjectPath = "/com/github/chjj/compton".ObjectPath

bus.GC_ref
var L: Lock

proc dbusReq*(callMethod: string; args: varargs[DbusValue]): Reply =
  L.acquire
  let msg: Message = dbus.makeCall(dst, obj, "com.github.chjj.compton", callMethod)
  for x in args: dbus.append(msg, x)
  result = dbus.waitForReply dbus.sendMessageWithReply(bus, msg)
  L.release
  dbus.raiseIfError(result)

proc getFocusedWnd(): uint32 {.inline.} =
  let reply = dbusReq("find_win", dbus.asDbusValue("focused"))
  var iter = dbus.iterate reply
  result = dbus.unpackCurrent(iter, uint32);
  dbus.ensureEnd(iter); dbus.close(reply)

proc setState*(wnd: Option[uint32]; state: State, failProtect: bool = false) =
  let curWnd = if wnd.isNone: getFocusedWnd() else: wnd.get
  var newState: bool

  try:
    var isOldCompton: bool = false

    if state != toggle:
      newState = state == State.on

    else:
      let reply = dbusReq(
        "win_get", dbus.asDbusValue(curWnd),
        dbus.asDbusValue("invert_color_force"),
      )
      var iter = dbus.iterate(reply)

      try:
        newState = dbus.unpackCurrent(iter, uint32) != 1
      except FieldDefect:
        isOldCompton = true
        newState = dbus.unpackCurrent(iter, uint16) != 1

      dbus.ensureEnd iter
      dbus.close reply

    dbus.close dbusReq(
      "win_set",
      dbus.asDbusValue curWnd,
      dbus.asDbusValue "invert_color_force",
      if not isOldCompton: dbus.asDbusValue newState.uint32
      else: dbus.asDbusValue newState.uint16
    )

  except DbusRemoteException:
    if failProtect:
      stderr.writeLine(
        "Prevented fail by remote DBus exception: " & getCurrentExceptionMsg())
    else:
      raise

initLock L
