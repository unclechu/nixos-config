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
    ${nix-utils.shellCheckers.fileIsExecutable "${pkgs.inotify-tools}/bin/inotifywait"}
    ${nix-utils.shellCheckers.fileIsExecutable "${pkgs.gnused}/bin/sed"}
    ${nix-utils.shellCheckers.fileIsExecutable "${pkgs.dzen2}/bin/dzen2"}
  '';
in
writeCheckedExecutable name checkPhase ''
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
''
