{ pkgs       ? import <nixpkgs> {}
, utils      ? import (import ../nix/sources.nix).nix-utils { inherit pkgs; }
, wenzels-i3 ? import ../apps/wenzels-i3.nix { inherit pkgs; }
}:
assert builtins.isPath wenzels-i3.rc || pkgs.lib.isDerivation wenzels-i3.rc;
let
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;
  src = builtins.readFile "${wenzels-i3.rc}/apps/${name}.pl";
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  perl = "${pkgs.perl}/bin/perl";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable perl}
    ${utils.shellCheckers.fileIsExecutable "${pkgs.xlibs.xrandr}/bin/xrandr"}
    ${utils.shellCheckers.fileIsExecutable "${pkgs.xdotool}/bin/xdotool"}
  '';
in
writeCheckedExecutable name checkPhase ''
  #! ${perl}
  use v5.10; use strict; use warnings;
  $ENV{PATH} = q<${pkgs.xlibs.xrandr}/bin:>.$ENV{PATH};
  $ENV{PATH} = q<${pkgs.xdotool}/bin:>.$ENV{PATH};
  ${src}
''
