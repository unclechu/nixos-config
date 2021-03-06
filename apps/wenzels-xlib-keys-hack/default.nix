# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../../nix/sources.nix; in
{ lib
, callPackage
, bash

# Overridable dependencies
, __nix-utils      ? callPackage sources.nix-utils      {}
, __xlib-keys-hack ? callPackage sources.xlib-keys-hack {}

# Build options
, __srcScript ? ./main.bash
}:
assert lib.isDerivation __xlib-keys-hack;
let
  inherit (__nix-utils) esc writeCheckedExecutable nameOfModuleWrapDir shellCheckers;
  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile __srcScript;
  bash-exe = "${bash}/bin/bash";
  xlib-keys-hack-exe = "${__xlib-keys-hack}/bin/xlib-keys-hack";

  checkPhase = ''
    ${shellCheckers.fileIsExecutable bash-exe}
    ${shellCheckers.fileIsExecutable xlib-keys-hack-exe}
  '';
in
writeCheckedExecutable name checkPhase ''
  #! ${bash-exe}
  set -Eeuo pipefail || exit
  exec <&-
  PATH=${esc __xlib-keys-hack}/bin:$PATH

  # Guard dependencies
  >/dev/null type -P grant-access-to-input-devices
  >/dev/null type -P ${baseNameOf xlib-keys-hack-exe}

  ${src}
''
