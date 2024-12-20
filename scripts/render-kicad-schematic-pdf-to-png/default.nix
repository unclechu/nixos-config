# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ lib
, writeTextFile
, bash
, ghostscript # For PDF → PNG conversion using ImageMagick
, imagemagick
}:
let
  executables = {
    bash = bash;
    gs = ghostscript;
    magick = imagemagick;
  };

  esc = lib.escapeShellArg;
  bin = pkg: exe: "${pkg}/bin/${exe}";
  e = builtins.mapAttrs (n: v: esc (bin v n)) executables;
  executableFileCheck = x: "[[ -f ${x} || -r ${x} || -x ${x} ]]";
in
writeTextFile rec {
  name = "render-kicad-schematic-pdf-to-png";
  executable = true;
  destination = "/bin/${name}";
  checkPhase = ''(
    set -o nounset
    ${builtins.concatStringsSep "\n" (map (x: ''
      if ! ${executableFileCheck x}; then (set -o xtrace && ${executableFileCheck x}); fi
    '') (builtins.attrValues e))}
  )'';
  text = ''
    #! ${let n = "bash"; in bin executables.${n} n}
    set -o errexit || exit

    export PATH=${
      esc (lib.makeBinPath (builtins.attrValues executables))
    }''${PATH:+:}''${PATH}

    ${builtins.readFile ./render-kicad-schematic-pdf-to-png.sh}
  '';
}
