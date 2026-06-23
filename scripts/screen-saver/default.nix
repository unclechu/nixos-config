# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ lib
, callPackage
, xset

, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../../utils/mk-generic-script.nix {}
}:

let
  e = executable-dependencies {
    xset = xset;
  };
in

mk-generic-script {
  name = "screen-saver";
  src = ./screen-saver.sh;
  inherit e;
}
