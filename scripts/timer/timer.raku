#! /usr/bin/env raku
# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/bashrc/master/LICENSE
use v6.d;
close $*IN;
my Int:D \LineLimit := 60;
my Str:D \DefaultMessage := 'Time is up!';
my Str:D \BS := "\c[BACKSPACE]";
my Str:D \UP := "\c[ESCAPE][1A";
my Int:D \Lines := 3;

class NotificationInteface {
  # A promise that resolves only when the desktop notification is closed
  has Promise:D $.notification-close-promise is required;
  # A callback that forcefully closes shown desktop notification (idempotent)
  has Callable:D $.close-notification is required;
}

# Show desktop notification
sub show-message(Str:D $message --> NotificationInteface:D) {
  my Int:D $notification-id = do {
    my Proc::Async:D $spawn-notification-proc := Proc::Async.new(
      :w,
      :close-stdin,
      |Q:w<dunstify --printid -u critical -- Timer>, $message
    );

    my Promise:D $notification-id-promise = Promise.new;
    my Str:D $buf = '';

    my Tap:D $spawn-notification-tap =
      $spawn-notification-proc.stdout.tap(-> Str:D $chunk {
        $buf ~= $chunk;

        # dunstify prints the ID and a newline; grab the first full line
        if $notification-id-promise.status !~~ Kept && $buf.contains("\n") {
          my $line = $buf.lines[0].trim;
          if $line ~~ /^\d+$/ {
            $notification-id-promise.keep($line.Int);
          } else {
            $notification-id-promise.break(
              "Unexpected dunstify output: {$line.raku}"
            );
          }
        }
      });

    my Promise:D $spawn-notification-promise := $spawn-notification-proc.start;
    await $spawn-notification-promise;
    my Int:D $notification-id = await $notification-id-promise;
    $spawn-notification-tap.close;
    $notification-id;
  };

  my Promise:D $notification-close-promise = do {
    my Proc::Async:D $monitor-proc = Proc::Async.new(
      :r,
      :close-stdin,
      'dbus-monitor',
      "interface='org.freedesktop.Notifications',member='NotificationClosed'"
    );

    my Promise:D $notification-closed-promise = Promise.new;
    my Bool:D $notification-closed-signal = False;

    my Tap:D $monitor-tap = $monitor-proc.stdout.lines.tap(-> Str:D $line {
      # dbus-monitor prints a "signal ..." header line for each signal.
      # Next line is the notification ID. Setting the variable so next
      # iteration we can now that we need to read the notification ID.
      if $line.contains('member=NotificationClosed') {
        $notification-closed-signal = True;
      } else {
        # The next uint32 after the header is the notification id
        if $notification-closed-signal && $line ~~ /'uint32'\s+(\d+)/ {
          my Int:D $closed-notification-id = $0.Int;

          if $closed-notification-id == $notification-id
          && $notification-closed-promise.status !~~ Kept {
            $notification-closed-promise.keep($closed-notification-id);

            # Stop monitoring once we've seen our close event
            $monitor-tap.close;
            $monitor-proc.kill;
          }

          # Either way, we're done matching this signal's id line
          $notification-closed-signal = False;
        }
      }
    });

    $monitor-proc.start;
    $notification-closed-promise;
  };

  return NotificationInteface.new(
    :notification-close-promise($notification-close-promise)
    :close-notification(-> { run(«dunstctl close "{$notification-id}"»).sink; })
  );
}

sub set-window-title(Str:D $title --> Nil) {
  $*OUT.print("\e]0;{$title}\a");
  $*OUT.flush;
}

sub view-duration(Duration:D $dur is copy, Str:D $pfx --> Str:D) {
  sub s(Int:D $x, Str:D $c --> Str:D) {
    return '' if $x == 0;
    " $x $c" ~ ($x == 1 ?? '' !! 's');
  }

  my Int:D $r-days := floor $dur / 60 / 60 / 24;
  $dur -= $r-days * 60 * 60 * 24;
  my Int:D $r-hours := floor $dur / 60 / 60;
  $dur -= $r-hours * 60 * 60;
  my Int:D $r-minutes := floor $dur / 60;
  $dur -= $r-minutes * 60;
  my Int:D $r-seconds := floor $dur;

  my Str:D $view = "{$pfx}: " ~ ((
    s($r-days, 'day'),
    s($r-hours, 'hour'),
    s($r-minutes, 'minute'),
    s($r-seconds, 'second'),
  ).join.&{$_ || ' '}.substr(1)) ~ (
    (($r-days + $r-hours + $r-minutes + $r-seconds) == 0)
      ?? ($dur > 0 ?? '<1 second' !! '0 seconds')
      !! ''
  );

  $view ~ ' ' x LineLimit - $view.chars;
}

sub MAIN(Str:D :m(:$message) = DefaultMessage, Bool:D :s(:$silent) = False, *@delays) {
  my Int:D $delay = 0;

  for @delays {
    when /^ (\d+) (d|h|m|s)? $/ {
      given $1 {
        when 'd' {$delay += $0 * 60 * 60 * 24}
        when 'h' {$delay += $0 * 60 * 60}
        when 'm' {$delay += $0 * 60}
        when $_ ~~ 's' || $_ ~~ Nil {$delay += $0}
        default {die "Unexpected value: $1"}
      }
    }

    default {die "Incorrect argument: $_"}
  }

  my DateTime:D $start-time := DateTime.now;
  my DateTime:D $delay-time := DateTime.new: $start-time.posix + $delay + 1;
  my DateTime:D $now = $start-time;
  print ("\n" x Lines) unless $silent;

  sub wait() {
    sleep $delay-time.posix - $now.posix - 1 > 0 ?? 1 !! 0.1;
    $now = DateTime.now;
  }

  my Duration:D $initial-timer = $delay-time - $start-time;

  while $delay-time > $now {
    {wait; next} if $silent;
    my Duration:D $spent := $now - $start-time;
    my Duration:D $remains := $delay-time - $now;
    my Str:D @parts = [
      view-duration($initial-timer, 'Timer'),
      view-duration($spent, 'Time passed'),
      view-duration($remains, 'Time remains'),
    ];
    say UP x Lines ~ @parts.join("\n");
    set-window-title @parts.map(*.trim).join(', ');
    wait;
  }

  say UP ~ DefaultMessage ~ ' ' x LineLimit - DefaultMessage.chars;
  my NotificationInteface:D $notification = show-message($message);

  my Promise:D $overspent-promise := $silent ?? Promise.new !! start {
    my DateTime:D $time-is-up-time := DateTime.now;

    loop {
      sleep 1;
      $now = DateTime.now;
      my Str:D $msg = view-duration($now - $time-is-up-time, 'Timer overdue by');
      say UP ~ $msg;
      set-window-title
        [view-duration($initial-timer, 'Timer'), $msg].map(*.trim).join(', ');
    }
  };

  sub on-exit() {
    set-window-title '';
    $notification.close-notification.();
  }

  END { on-exit; }
  signal(SIGINT).tap(-> $ { on-exit; exit 130; });
  signal(SIGTERM).tap(-> $ { on-exit; exit 143; });

  await Promise.anyof(
    $overspent-promise,
    $notification.notification-close-promise,
  );
}
