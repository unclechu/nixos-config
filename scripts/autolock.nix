# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ lib
, callPackage
, writeText
, dash
, xautolock
, i3lock
, procps

# Overridable dependencies
, executable-dependencies ? callPackage ../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../utils/mk-generic-script.nix {}

# Build options
, minutes ? 5
}:

let
  name = "autolock";

  e = executable-dependencies {
    dash = dash;
    xautolock = xautolock;
    i3lock = i3lock;
    pkill = procps;
  };

  src = writeText "${name}-source" ''
    #! /usr/bin/env dash
    exec <&-

    ${e.s.pkill} -x -U "$USER" -- ${lib.escapeShellArg (baseNameOf e.b.xautolock)} 2>/dev/null

    ${e.s.xautolock} \
      -time ${lib.escapeShellArg minutes} \
      -locker ${lib.escapeShellArg "${e.s.i3lock} -c 111111"} \
      1>/dev/null 2>/dev/null &
  '';
in

mk-generic-script {
  inherit name src e;
  dontAddDependencies = true;
  buildInputs = [ e.executables.dash ];
}
