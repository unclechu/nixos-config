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
    ${utils.shellCheckers.fileIsExecutable "${pkgs.inotify-tools}/bin/inotifywait"}
    ${utils.shellCheckers.fileIsExecutable "${pkgs.gnused}/bin/sed"}
    ${utils.shellCheckers.fileIsExecutable "${pkgs.dzen2}/bin/dzen2"}
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
