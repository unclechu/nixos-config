# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ callPackage
, writeText
, bash
, dconf

, executable-dependencies ? callPackage ../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../utils/mk-generic-script.nix {}
}:

let
  name = "pulseaudio-share-server";

  e = executable-dependencies {
    bash = bash;
    dconf = dconf;
  };

  src = writeText "${name}-source" ''
    #! /usr/bin/env bash
    set -o errexit || exit; set -o errtrace; set -o pipefail; set -o nounset

    DIR=/org/freedesktop/pulseaudio/module-groups/remote-access/

    if (( $# == 0 )) || ( (( $# == 1 )) && [[ $1 == on ]] ); then
      >&2 echo 'Turning on…'
      enabled=true
    elif (( $# == 1 )) && [[ $1 == off ]]; then
      >&2 echo 'Turning off…'
      enabled=false
    elif (( $# == 1 )) && [[ $1 == '--help' || $1 == '-h' ]]; then
      printf 'Usage: %s [on|off]\n' "$0"
      exit 0
    else
      >&2 printf 'Incorrect %d argument(s): %s\n' "$#" "$*"
      >&2 printf 'Usage: %s [on|off]\n' "$0"
      exit 1
    fi

    ${e.s.dconf} load "$DIR" << EOF
      [/]
      args0='auth-anonymous=1'
      args1='auth-anonymous=1'
      enabled=$enabled
      name0='module-native-protocol-tcp'
      name1='module-esound-protocol-tcp'
    EOF
  '';
in

mk-generic-script {
  inherit name e src;
  dontAddDependencies = true;
  cutOffRuntimeDependenciesCheckPhase = null;
}
