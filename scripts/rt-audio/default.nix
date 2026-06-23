# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ callPackage
, jack2
, jack-example-tools

, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../../utils/mk-generic-script.nix {}
}:

let
  e = executable-dependencies {
    jack_control = jack2;
    jack_bufsize = jack-example-tools;
  };
in

mk-generic-script {
  name = "rt-audio";
  src = ./rt-audio.sh;
  inherit e;
}
