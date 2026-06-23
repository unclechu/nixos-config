# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ callPackage
, writeText
, dash

# Overridable dependencies
, executable-dependencies ? callPackage ../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../utils/mk-generic-script.nix {}
, __wenzels-keyboard ? callPackage ./wenzels-keyboard {}
, __pointers-setup ? callPackage ./pointers-setup.nix {}
}:
let
  name = "input-setup";

  e = executable-dependencies {
    dash = dash;
    wenzels-keyboard = __wenzels-keyboard;
    pointers-setup = __pointers-setup;
  };

  src = writeText "${name}-source" ''
    #! /usr/bin/env dash
    exec <&- 1>/dev/null 2>/dev/null # silent

    # keyboard
    ${e.s.wenzels-keyboard} &

    # mouse
    ${e.s.pointers-setup}

    exit 0 # prevent returning exit status of the latest command
  '';
in

mk-generic-script {
  inherit name src e;
  dontAddDependencies = true;
  buildInputs = [ e.executables.dash ];
}
