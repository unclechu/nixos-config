# TODO this is 99% identical to ../pointer-logitech-wireless-ambidextrous-small-mouse
#      implement generic solution
args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, ...
}:
let
  utils = import ../../utils args;
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
