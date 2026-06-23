# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ callPackage
, lib
, pulseaudio
, gnugrep
, gnused
, gawk
, findutils
, coreutils

# Overridable dependencies
, __dzen-box ? callPackage ../dzen-box {}
, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../../utils/mk-generic-script.nix {}
}:

let
  e = executable-dependencies {
    pacmd = pulseaudio;
    pactl = pulseaudio;
    grep = gnugrep;
    sed = gnused;
    awk = gawk;
    xargs = findutils;
    basename = coreutils;
    dzen-box = __dzen-box;
  };
in

mk-generic-script {
  name = "pamng";
  src = ./pamng.sh;
  inherit e;
}
