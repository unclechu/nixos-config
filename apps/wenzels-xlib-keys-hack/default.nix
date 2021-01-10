let sources = import ../../nix/sources.nix; in
{ pkgs           ? import <nixpkgs> {}
, utils          ? import sources.nix-utils { inherit pkgs; }
, xlib-keys-hack ? import sources.xlib-keys-hack { inherit pkgs; }
}:
assert pkgs.lib.isDerivation xlib-keys-hack;
let
  inherit (utils) esc writeCheckedExecutable nameOfModuleWrapDir;
  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.bash;
  bash = "${pkgs.bash}/bin/bash";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable bash}
    ${utils.shellCheckers.fileIsExecutable "${xlib-keys-hack}/bin/xlib-keys-hack"}
  '';
in
writeCheckedExecutable name checkPhase ''
  #! ${bash}
  set -Eeuo pipefail
  exec <&-
  PATH=${esc xlib-keys-hack}/bin:$PATH

  # guard dependencies
  >/dev/null which grant-access-to-input-devices
  >/dev/null which xlib-keys-hack
  ${src}
''
