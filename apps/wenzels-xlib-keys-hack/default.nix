args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, xlib-keys-hack
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
    ${utils.shellCheckers.fileIsExecutable "${xlib-keys-hack}/bin/xlib-keys-hack"}
  '';

  pkg = writeCheckedExecutable name checkPhase ''
    #! ${bash}
    set -Eeuo pipefail
    exec <&-
    PATH=${esc xlib-keys-hack}/bin:$PATH

    # guard dependencies
    >/dev/null which grant-access-to-input-devices
    >/dev/null which xlib-keys-hack
    ${src}
  '';
in
{
  inherit name pkg checkPhase;
}
