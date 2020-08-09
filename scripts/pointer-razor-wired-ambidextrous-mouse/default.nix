# TODO this is 99% identical to ../pointer-logitech-wireless-ambidextrous-small-mouse
#      implement generic solution
args@{ ... }:
let pkgs-k = "pkgs"; utils-k = "utils"; config-k = "config"; in
assert let k = pkgs-k;  in builtins.hasAttr k args -> builtins.isAttrs args.${k};
assert let k = utils-k; in builtins.hasAttr k args -> builtins.isAttrs args.${k};
let
  pkgs = args.${pkgs-k} or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { ${k} = args.${k}.nixpkgs.${k}; } else {}
  ));

  utils = args.${utils-k} or (import ../../picks/nix-utils.nix (
    pkgs.lib.filterAttrs (k: _: k == config-k) args // { inherit pkgs; }
  )).pkg;

  inherit (utils) esc writeCheckedExecutable nameOfModuleWrapDir;

  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.bash;

  bash = "${pkgs.bash}/bin/bash";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable bash}
    ${utils.shellCheckers.fileIsExecutable "${pkgs.gnugrep}/bin/grep"}
    ${utils.shellCheckers.fileIsExecutable "${pkgs.xlibs.xinput}/bin/xinput"}
  '';

  pkg = writeCheckedExecutable name checkPhase ''
    #! ${bash}
    set -Eeuo pipefail
    exec <&-
    export PATH=${esc pkgs.gnugrep}/bin:${esc pkgs.xlibs.xinput}/bin:$PATH

    # guard dependencies
    >/dev/null which xinput
    >/dev/null which grep
    ${src}
  '';
in
{
  inherit name pkg checkPhase;
}
