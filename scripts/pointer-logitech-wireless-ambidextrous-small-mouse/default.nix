# TODO this is 99% identical to ../pointer-razor-wired-ambidextrous-mouse,
#      implement generic solution
let sources = import ../../nix/sources.nix; in
{ pkgs
, nix-utils ? pkgs.callPackage sources.nix-utils {}
}:
let
  inherit (nix-utils) esc writeCheckedExecutable nameOfModuleWrapDir;
  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.bash;
  bash = "${pkgs.bash}/bin/bash";

  checkPhase = ''
    ${nix-utils.shellCheckers.fileIsExecutable bash}
    ${nix-utils.shellCheckers.fileIsExecutable "${pkgs.gnugrep}/bin/grep"}
    ${nix-utils.shellCheckers.fileIsExecutable "${pkgs.xlibs.xinput}/bin/xinput"}
  '';
in
writeCheckedExecutable name checkPhase ''
  #! ${bash}
  set -Eeuo pipefail || exit
  exec <&-
  export PATH=${esc pkgs.gnugrep}/bin:${esc pkgs.xlibs.xinput}/bin:$PATH

  # Guard dependencies
  >/dev/null type -P xinput
  >/dev/null type -P grep
  ${src}
''
