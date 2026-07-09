# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

from std/times import Duration, initDuration
from std/monotimes import MonoTime, `+`, `<=`, getMonoTime
from std/os import sleep

const
  pollTimeout: Duration = initDuration(seconds = 5)
  pollIntervalMs: uint = 100

template getPollDeadline(): MonoTime = getMonoTime() + pollTimeout

# Wait for the condition expression to result to `true` and result into `true`
# or to `false` if `pollTimeout` is hit.
template waitForCondition*(condition: bool): bool =
  block:
    let deadline: MonoTime = getPollDeadline()
    var satisfied: bool = true
    while not (condition):
      if deadline <= monotimes.getMonoTime(): (satisfied = false; break)
      sleep(pollIntervalMs.int)
    satisfied
