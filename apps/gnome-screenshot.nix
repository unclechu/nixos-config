# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ callPackage
, writeText
, dash
, coreutils
, gnome-screenshot

# Overridable dependencies
, executable-dependencies ? callPackage ../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../utils/mk-generic-script.nix {}
}:

let
  name = "gnome-screenshot";

  e = executable-dependencies {
    dash = dash;
    gnome-screenshot = gnome-screenshot;
    date = coreutils;
  };

  src = writeText "${name}-source" ''
    #! /usr/bin/env dash
    DATE=$(${e.s.date} +'%Y-%m-%d %H-%M-%S') || exit
    ${e.s.gnome-screenshot} --file="$HOME/Pictures/Screenshots/$DATE.png" "$@"
  '';
in

mk-generic-script {
  inherit name src e;
  dontAddDependencies = true;
  buildInputs = [ e.executables.dash ];
}
