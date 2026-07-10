# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# Create OS-level timers using timerfd API.
#
# Usage examples:
#
#   var timer = newTimerFd()
#   defer: timer.close()
#
#   # Setting the timer and waiting for the expiry:
#   timer.arm(1000) # start / reset to 1 second
#   timer.wait() # blocks until expiry
#   echo "timer expired"
#
#   # Resetting is just calling arm again:
#   timer.arm(1000)
#   timer.arm(1000) # resets the same timer
#
#   # Cancelling:
#   timer.disarm()

from std/posix import close, read, Time
from std/oserrors import osLastError, raiseOSError

type
  TimerFd* {.requiresInit.} = object
    fd*: cint

  Timespec {.importc: "struct timespec", header: "<time.h>".} = object
    tv_sec: Time
    tv_nsec: clong

  Itimerspec {.importc: "struct itimerspec", header: "<sys/timerfd.h>".} = object
    it_interval: Timespec
    it_value: Timespec

# See https://linux.die.net/man/2/timerfd_create
proc timerfd_create(clockid: cint, flags: cint): cint
  {.importc: "timerfd_create", header: "<sys/timerfd.h>".}

# See https://linux.die.net/man/2/timerfd_settime
proc timerfd_settime(
  fd: cint,
  flags: cint,
  newValue: ptr Itimerspec,
  oldValue: ptr Itimerspec,
): cint
  {.importc: "timerfd_settime", header: "<sys/timerfd.h>".}

let
  CLOCK_MONOTONIC {.importc: "CLOCK_MONOTONIC", header: "<time.h>".}: cint
  TFD_CLOEXEC {.importc: "TFD_CLOEXEC", header: "<sys/timerfd.h>".}: cint

proc newTimerFd*(): TimerFd {.inline.} =
  let fd: cint = timerfd_create(CLOCK_MONOTONIC, TFD_CLOEXEC)
  if fd < 0: raiseOSError(osLastError(), "timerfd_create")
  TimerFd(fd: fd)

# Set the timer in milliseconds.
#
# Note that setting the timer to `0` resets it, so `wait` blocks indefinitely
# until you re-`arm` the timer to a non-zero value.
proc arm*(timer: TimerFd; ms: uint): void {.inline.} =
  var spec = Itimerspec(
    it_interval: Timespec(tv_sec: 0.Time, tv_nsec: 0.clong), # one-shot
    it_value: Timespec(
      tv_sec: Time(ms div 1000),
      tv_nsec: clong((ms mod 1000) * 1_000_000),
    ),
  )

  if timerfd_settime(timer.fd, 0, spec.addr, nil) != 0:
    raiseOSError(osLastError(), "timerfd_settime")

# Cancel timer.
#
# Note that it does not interrupt `wait`.
# If you `disarm` `wait` will be blocked indefinitely
# unless you `arm` the timer again.
#
# For waking the `wait` use `arm(1)` (1ms delay).
proc disarm*(timer: TimerFd): void {.inline.} =
  var spec: Itimerspec = Itimerspec()
  # Zero value disables the timer
  if timerfd_settime(timer.fd, 0, spec.addr, nil) != 0:
    raiseOSError(osLastError(), "timerfd_settime disarm")

# Block until timer expiry.
#
# It is safe to `wait` in one thread and `arm`/`disarm` in another.
proc wait*(timer: TimerFd): void {.inline.} =
  var expirations: uint64
  let n = read(timer.fd, expirations.addr, sizeof(expirations))
  if n != sizeof(expirations): raiseOSError(osLastError(), "read timerfd")

# Close the timer.
#
# Just descriptor closing.
# Make sure your `wait`s are done when calling `close`.
# You can do `arm(1)` to wake the `wait` and use some kind of stopping flag.
# And report back when the `wait` is done so that you can `close`.
proc close*(timer: var TimerFd): void {.inline.} =
  if timer.fd >= 0: (discard close(timer.fd); timer.fd = -1)
