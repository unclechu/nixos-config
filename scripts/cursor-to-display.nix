args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, ...
}:
let
  utils = import ../utils args;
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;

  src = builtins.readFile "${(import ../apps/wenzels-i3.nix args).rc}/apps/${name}.pl";
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  perl = "${pkgs.perl}/bin/perl";

  checkPhase = ''
    ${utils.bash.checkFileIsExecutable perl}
    ${utils.bash.checkFileIsExecutable "${pkgs.xlibs.xrandr}/bin/xrandr"}
    ${utils.bash.checkFileIsExecutable "${pkgs.xdotool}/bin/xdotool"}
  '';

  pkg = writeCheckedExecutable name checkPhase ''
    #! ${perl}
    use v5.10; use strict; use warnings;
    $ENV{PATH} = q<${pkgs.xlibs.xrandr}/bin:>.$ENV{PATH};
    $ENV{PATH} = q<${pkgs.xdotool}/bin:>.$ENV{PATH};
    ${src}
  '';
in
{
  inherit name src pkg checkPhase;
}
