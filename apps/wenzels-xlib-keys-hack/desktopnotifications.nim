# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

from subproc import spawn

from log import Log, debug
from needexe import nil

template notify*[TimeFormat: static string, Writer](
  logger: lent Log[TimeFormat, Writer],
  title: string = "wenzels-xlib-keys-hack",
  text: string = "",
  urgent: bool = false,
): void =
  block:
    var args: seq[string] = @[]
    if urgent: args.add(@["-u", "critical"])
    args.add(@["--", title])
    if text.len > 0: args.add(text)
    spawn(logger, Command(cmd: needexe.notifySend, args: args))
