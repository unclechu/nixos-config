# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ callPackage
, writeText
, bash

# Overridable dependencies
, __dzen-box ? callPackage ./dzen-box {}
, executable-dependencies ? callPackage ../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../utils/mk-generic-script.nix {}
}:

let
  name = "screen-backlight";

  e = executable-dependencies {
    bash = bash;
    dzen-box = __dzen-box;
  };

  src = writeText "${name}-source" ''
    #! /usr/bin/env bash
    set -o errexit || exit
    exec <&-

    # Guard dependencies
    >/dev/null type laptop-backlight

    if (( $# != 1 )) || ! [[ $1 =~ ^(-|\+)?([0-9]+)%$ ]]; then
      (
        echo
        printf 'Incorrect argument: “%s”\n' "$@"
        echo "  $0 50%"
        echo "  $0 +10%"
        echo "  $0 -10%"
        echo
      ) >&2
      exit 1
    fi

    MAX=$(laptop-backlight get-max)
    OPERATOR=''${BASH_REMATCH[1]}
    val=$(( BASH_REMATCH[2] * MAX / 100 ))

    if [[ -n "$OPERATOR" ]]; then
      CUR=$(laptop-backlight get)
      if [[ "$OPERATOR" == - ]]; then
        val=$(( CUR - val ))
      elif [[ "$OPERATOR" == + ]]; then
        val=$(( CUR + val ))
      fi
    fi

    if (( val < 0 )); then val=0
    elif (( val > MAX )); then val=$MAX; fi

    ${e.s.dzen-box} $(( val * 100 / MAX ))% yellow
    laptop-backlight set "$val"
  '';
in

mk-generic-script {
  inherit name e src;
  dontAddDependencies = true;
  cutOffRuntimeDependenciesCheckPhase = null;
}
