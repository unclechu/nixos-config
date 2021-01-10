# TODO this is 99% identical to ../pointer-razor-wired-ambidextrous-mouse,
#      implement generic solution
{ pkgs  ? import <nixpkgs> {}
, utils ? import (import ../../nix/sources.nix).nix-utils { inherit pkgs; }
}:
let
  inherit (utils) esc writeCheckedExecutable nameOfModuleWrapDir;
  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.bash;
  bash = "${pkgs.bash}/bin/bash";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable bash}
    ${utils.shellCheckers.fileIsExecutable "${pkgs.gnugrep}/bin/grep"}
    ${utils.shellCheckers.fileIsExecutable "${pkgs.xlibs.xinput}/bin/xinput"}
  '';
in
writeCheckedExecutable name checkPhase ''
  #! ${bash}
  set -Eeuo pipefail
  exec <&-
  export PATH=${esc pkgs.gnugrep}/bin:${esc pkgs.xlibs.xinput}/bin:$PATH

  # guard dependencies
  >/dev/null which xinput
  >/dev/null which grep
  ${src}
''
