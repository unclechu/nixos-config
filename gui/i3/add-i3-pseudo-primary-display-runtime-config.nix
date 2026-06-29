# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../../nix/sources.nix; in

{ callPackage

, jq
, i3
, procps

, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../../utils/mk-generic-script.nix {}
}:

let
  e = executable-dependencies {
    jq = jq;
    i3-msg = i3;
    pidof = procps;
  };
in

mk-generic-script {
  name = "add-i3-pseudo-primary-display-runtime-config";
  src = ./add-i3-pseudo-primary-display-runtime-config.sh;
  inherit e;
}
