args@{ ... }:
let pkgs-k = "pkgs"; utils-k = "utils"; config-k = "config"; in
assert let k = pkgs-k;  in builtins.hasAttr k args -> builtins.isAttrs args.${k};
assert let k = utils-k; in builtins.hasAttr k args -> builtins.isAttrs args.${k};
let
  pkgs = args.${pkgs-k} or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { ${k} = args.${k}.nixpkgs.${k}; } else {}
  ));

  utils = args.${utils-k} or (import ../../nix-utils-pick.nix (
    pkgs.lib.filterAttrs (k: _: k == config-k) args // { inherit pkgs; }
  )).pkg;

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
