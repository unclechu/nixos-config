# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

my $LOCKFILE := %*ENV{'HOME'}.IO.child: '.wenzels-keyboard.lock';
my @pkill = (pkill, '-x', '-U', %*ENV{'USER'}, '--');

sub MAIN( Bool :f(:$force) = False
        , Bool :v(:$verbose) = False
        , Bool :$internal-bg = False
        , Bool :$no-xlib-hack = False
        )
{
  my &s = &safe-spawn.assuming: $verbose;

  if $internal-bg.not {

    if !$force && $LOCKFILE.f && how-old-is-lockfile() < 60 {
      note "Couldn't start, lock file exists: '$LOCKFILE'!\n"
         , "Start with [-f|--force] to force start.";
      exit 1;
    } else {
      $LOCKFILE.IO.open(:w).close;
    }

    s True, xset, 'r', 'rate', keyRepeatDelay, keyRepeatInterval;
    s True, setxkbmap, '-layout', xkbLayout, '-option', xkbOptions;

    s True, numlockx, 'off';
    s True, numlockx, 'on';

    "'$*PROGRAM' will do last part in background…".note if $verbose;

    given Proc::Async.new(
      :w, $*EXECUTABLE, $*PROGRAM, |@*ARGS.append: '--internal-bg')
    { .start; .close-stdin; exit 0; }

  } else {

    "'$*PROGRAM' is waiting for promises…".note if $verbose;

    await Promise.allof(
      start {
        "'$*PROGRAM' is restarting 'xbindkeys' in own thread…".note if $verbose;
        s False, @pkill, 'xbindkeys';
        await Promise.in: 1;
        s True, xbindkeys;
        "'$*PROGRAM' is done with restarting 'xbindkeys'.".note if $verbose;
      },

      start {
        my Str:D \xlib-keys-hack-starter-name = xlib-keys-hack-starter.IO.basename;

        if $no-xlib-hack {
          ("'$*PROGRAM' is stopping '"~xlib-keys-hack-starter-name~"' in own thread…").note
            if $verbose;
        } else {
          ("'$*PROGRAM' is restarting '"~xlib-keys-hack-starter-name~"' in own thread…").note
            if $verbose;
        }

        s False, @pkill, xlib-keys-hack-starter-name;
        s False, @pkill, 'xlib-keys-hack';
        s False, @pkill, 'xlib-keys-hack-watch-for-window-focus-events';

        if $no-xlib-hack {
          ("'$*PROGRAM' is done with stopping '"~xlib-keys-hack-starter-name~"'.").note if $verbose;
        } else {
          await Promise.in: 1;
          walking-zombie $verbose, xlib-keys-hack-starter;
          "'$*PROGRAM' is done with restarting '"~xlib-keys-hack-starter-name~"'.".note if $verbose;
        }
      }
    );

    "'$*PROGRAM' is done waiting for promises.".note if $verbose;
    "'$*PROGRAM' is removing its lockfile: '$LOCKFILE'…".note if $verbose;
    $LOCKFILE.unlink;
    "'$*PROGRAM' is done.".note if $verbose;
  }
}

### helpers ###

sub walking-zombie(Bool $verbose, *@run-args) {
  my $cmd = @run-args.join: ' ';
  "'$*PROGRAM' is spawning zombie: '$cmd'…".note if $verbose;
  given Proc::Async.new(:w, |@run-args)
    { .stdout.tap.close; .stderr.tap.close; .start; .close-stdin; }
  "'$*PROGRAM' is done with spawning zombie: '$cmd'…".note if $verbose;
}

sub safe-spawn(Bool $verbose, Bool $warn, *@run-args) {
  my $timeout := 2;
  my $cmd = @run-args.join: ' ';
  ("'$*PROGRAM' is spawning: '$cmd'…").note if $verbose;

  my &runit = sub (Proc::Async $proc) {
    my $promise = $proc.start;
    $proc.close-stdin;
    return ($proc, $promise);
  };

  try {
    my ($proc, $promise) = runit Proc::Async.new(:w, |@run-args);

    await Promise.anyof(
      $promise,
      Promise.in($timeout).then: {
        my $what = (@run-args[0] == pkill) ?? 'dying' !! 'killing it';
        "'$*PROGRAM' command '$cmd' is out of time, $what…".note if $verbose;
        $proc.kill: SIGKILL;

        if @run-args[0] == pkill {
          my @args = @run-args;
          @args.shift;
          @args.prepend(pkill, '-KILL');
          $cmd = @args.join: ' ';

          "'$*PROGRAM' is spawning new killing command '$cmd'…".note
            if $verbose;

          my ($proc, $promise) = runit Proc::Async.new(:w, |@args);
          await Promise.anyof(
            $promise,
            Promise.in($timeout).then: {
              "'$*PROGRAM' command '$cmd' is out of time, dying…".note
                if $verbose;

              $proc.kill: SIGKILL;
              exit 1;
            }
          );
        } else {
          exit 1;
        }
      }
    );

    CATCH {
      default {
        "'$*PROGRAM' is failed spawning: '$cmd', error: $_.".note if $verbose;
        warn $_ if $warn;
      }
    }
  }

  "'$*PROGRAM' is done with spawning: '$cmd'.".note if $verbose;
}

sub how-old-is-lockfile() of Duration {
  (run 'stat', $LOCKFILE, :out).out.slurp ~~ /Modify\:\s (.+?) \n/;
  $_ = $0.split(' ');
  $_ = "$_[0]T" ~ $_[1].split('.')[0] ~ $_[2].subst(/(\d\d)(\d\d)/, {"$0:$1"});
  DateTime.now - DateTime.new: $_;
}
