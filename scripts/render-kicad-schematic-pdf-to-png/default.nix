# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ callPackage
, coreutils
, ghostscript # For PDF → PNG conversion using ImageMagick
, imagemagick

, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../../utils/mk-generic-script.nix {}
}:

let
  e = executable-dependencies {
    gs = ghostscript;
    magick = imagemagick;
    basename = coreutils;
  };
in

mk-generic-script {
  name = "render-kicad-schematic-pdf-to-png";
  src = ./render-kicad-schematic-pdf-to-png.sh;
  inherit e;
}
