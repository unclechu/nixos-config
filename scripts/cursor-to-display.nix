let sources = import ../nix/sources.nix; in
{ pkgs       ? import <nixpkgs> {}
, utils      ? import sources.nix-utils { inherit pkgs; }
, wenzels-i3 ? sources.i3rc
}:
assert
  builtins.isPath wenzels-i3 ||
  pkgs.lib.isDerivation wenzels-i3 ||
  builtins.isString wenzels-i3 ||
  builtins.isString wenzels-i3.outPath;
let
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;
  src = builtins.readFile "${wenzels-i3}/apps/${name}.pl";
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
