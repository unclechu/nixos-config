# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# TODO this is 99% identical to ../pointer-razor-wired-ambidextrous-mouse,
#      implement generic solution
let sources = import ../../nix/sources.nix; in
{ callPackage
, bash
, gnugrep
, xlibs # Just for ‘xinput’

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}

# Build options
, __srcScript ? ./main.bash
}:
let
  inherit (__nix-utils) esc writeCheckedExecutable nameOfModuleWrapDir shellCheckers;
  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile __srcScript;
  bash-exe = "${bash}/bin/bash";

  checkPhase = ''
    ${shellCheckers.fileIsExecutable bash-exe}
    ${shellCheckers.fileIsExecutable "${gnugrep}/bin/grep"}
    ${shellCheckers.fileIsExecutable "${xlibs.xinput}/bin/xinput"}
  '';
in
writeCheckedExecutable name checkPhase ''
  #! ${bash-exe}
  set -Eeuo pipefail || exit
  exec <&-
  export PATH=${esc gnugrep}/bin:${esc xlibs.xinput}/bin:$PATH

  # Guard dependencies
  >/dev/null type -P xinput
  >/dev/null type -P grep

  ${src}
''
