# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ lib
, callPackage
, writeText
, dash
, libnotify

# Overridable dependencies
, executable-dependencies ? callPackage ../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../utils/mk-generic-script.nix {}
}:

let
  name = "scream";

  e = executable-dependencies {
    dash = dash;
    notify-send = libnotify;
  };

  src = writeText "${name}-source" ''
    #! /usr/bin/env dash
    set -o errexit || exit; set -o nounset; set -o pipefail
    set -o xtrace
    exec ${e.s.notify-send} -u critical -- "$@"
  '';
in

mk-generic-script {
  inherit name e src;
  dontAddDependencies = true;
  cutOffRuntimeDependenciesCheckPhase = null;
}
