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
    ${utils.bash.checkFileIsExecutable bash}
    ${utils.bash.checkFileIsExecutable "${pkgs.inotify-tools}/bin/inotifywait"}
    ${utils.bash.checkFileIsExecutable "${pkgs.gnused}/bin/sed"}
    ${utils.bash.checkFileIsExecutable "${pkgs.dzen2}/bin/dzen2"}
  '';

  pkg = writeCheckedExecutable name checkPhase ''
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
  '';
in
{
  inherit name pkg checkPhase;
}
