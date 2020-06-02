args@{ ... }:
assert let k = "pkgs";  in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
assert let k = "utils"; in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
let
  pkgs = args.pkgs or (import <nixpkgs> {
    config = let k = "config"; in
      if builtins.hasAttr k args then {} else args."${k}".nixpkgs.config;
  });

  utils = args.utils or (import ../nix-utils-pick.nix args).pkg;
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;

  src = builtins.readFile "${(import ../apps/wenzels-i3.nix args).rc}/apps/${name}.pl";
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  perl = "${pkgs.perl}/bin/perl";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable perl}
    ${utils.shellCheckers.fileIsExecutable "${pkgs.xlibs.xrandr}/bin/xrandr"}
    ${utils.shellCheckers.fileIsExecutable "${pkgs.xdotool}/bin/xdotool"}
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
