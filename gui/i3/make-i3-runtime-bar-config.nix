# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../../nix/sources.nix; in

{ lib
, callPackage

, jq
, i3
, procps

, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../../utils/mk-generic-script.nix {}

, wenzels-i3-status-generator ? callPackage ./wenzels-i3-status-generator {}
}:

let
  e = executable-dependencies {
    jq = jq;
    i3-msg = i3;
    pidof = procps;
    wenzels-i3-status-generator = wenzels-i3-status-generator;
  };
in

mk-generic-script {
  name = "make-i3-runtime-bar-config";
  src = ./make-i3-runtime-bar-config.sh;
  inherit e;
  postPatch = ''
    substituteInPlace "$src" --replace-fail ${
      lib.escapeShellArg wenzels-i3-status-generator.meta.mainProgram
    } ${
      e.s.wenzels-i3-status-generator
    }
  '';
}
