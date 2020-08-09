args@{ ... }:
let pkgs-k = "pkgs"; utils-k = "utils"; config-k = "config"; in
assert let k = pkgs-k;  in builtins.hasAttr k args -> builtins.isAttrs args.${k};
assert let k = utils-k; in builtins.hasAttr k args -> builtins.isAttrs args.${k};
let
  pkgs = args.${pkgs-k} or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { ${k} = args.${k}.nixpkgs.${k}; } else {}
  ));

  utils = args.${utils-k} or (import ../picks/nix-utils.nix (
    pkgs.lib.filterAttrs (k: _: k == config-k) args // { inherit pkgs; }
  )).pkg;

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
