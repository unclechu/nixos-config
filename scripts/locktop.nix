# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ callPackage
, writeText
, dash
, xautolock
, i3lock
, jack2

# Overridable dependencies
, __dzen-box ? callPackage ./dzen-box {}
, wenzels-keyboard ? callPackage ./wenzels-keyboard {}
, executable-dependencies ? callPackage ../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../utils/mk-generic-script.nix {}
}:
let
  name = "locktop";

  e = executable-dependencies {
    dash = dash;
    xautolock = xautolock;
    i3lock = i3lock;
    jack_control = jack2;
    dzen-box = __dzen-box;
    wenzels-keyboard = wenzels-keyboard;
  };

  src = writeText "${name}-source" ''
    #! /usr/bin/env dash
    ${e.s.dzen-box} LOCK orangered
    ${e.s.wenzels-keyboard} --no-xlib-hack
    ${e.s.jack_control} stop
    if [ -x ~/.screenlayout/only-laptop.sh ]; then ~/.screenlayout/only-laptop.sh; fi
    if ! ${e.s.xautolock} -locknow; then ${e.s.i3lock} -c 111111; fi
  '';
in

mk-generic-script {
  inherit name src e;
  dontAddDependencies = true;
  buildInputs = [ e.executables.dash ];
}
