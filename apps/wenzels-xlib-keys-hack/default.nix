let sources = import ../../nix/sources.nix; in
{ lib
, callPackage
, bash

# Overridable dependencies
, __nix-utils      ? callPackage sources.nix-utils      {}
, __xlib-keys-hack ? callPackage sources.xlib-keys-hack {}
}:
assert lib.isDerivation __xlib-keys-hack;
let
  inherit (__nix-utils) esc writeCheckedExecutable nameOfModuleWrapDir shellCheckers;
  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.bash;
  bash-exe = "${bash}/bin/bash";
  xlib-keys-hack-exe = "${__xlib-keys-hack}/bin/xlib-keys-hack";

  checkPhase = ''
    ${shellCheckers.fileIsExecutable bash-exe}
    ${shellCheckers.fileIsExecutable xlib-keys-hack-exe}
  '';
in
writeCheckedExecutable name checkPhase ''
  #! ${bash-exe}
  set -Eeuo pipefail
  exec <&-
  PATH=${esc __xlib-keys-hack}/bin:$PATH

  # guard dependencies
  >/dev/null type grant-access-to-input-devices
  >/dev/null type ${baseNameOf xlib-keys-hack-exe}
  ${src}
''
