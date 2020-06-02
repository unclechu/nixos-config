args@{ ... }:
assert let k = "pkgs";  in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
assert let k = "utils"; in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
let
  pkgs = args.pkgs or (import <nixpkgs> {
    config = let k = "config"; in
      if builtins.hasAttr k args then {} else args."${k}".nixpkgs.config;
  });

  utils = args.utils or (import ../../nix-utils-pick.nix args).pkg;
  inherit (utils) esc writeCheckedExecutable nameOfModuleWrapDir;

  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.bash;
  bash = "${pkgs.bash}/bin/bash";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable bash}
    ${utils.shellCheckers.fileIsExecutable "${pkgs.inotify-tools}/bin/inotifywait"}
    ${utils.shellCheckers.fileIsExecutable "${pkgs.gnused}/bin/sed"}
    ${utils.shellCheckers.fileIsExecutable "${pkgs.dzen2}/bin/dzen2"}
  '';

  pkg = writeCheckedExecutable name checkPhase ''
    #! ${bash}
    set -e
    exec <&-

    PATH=${esc pkgs.inotify-tools}/bin:$PATH
    PATH=${esc pkgs.gnused}/bin:$PATH
    PATH=${esc pkgs.dzen2}/bin:$PATH

    # guard dependencies
    >/dev/null which inotifywait
    >/dev/null which sed
    >/dev/null which dzen2

    ${src}
  '';
in
{
  inherit name pkg checkPhase;
}
