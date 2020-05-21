args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, ...
}:
let
  utils = import ../utils args;
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;

  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  bash = "${pkgs.bash}/bin/bash";
  xautolock = "${pkgs.xautolock}/bin/xautolock";
  i3lock = "${pkgs.i3lock}/bin/i3lock";
  pkill = "${pkgs.procps}/bin/pkill";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable bash}
    ${utils.shellCheckers.fileIsExecutable xautolock}
    ${utils.shellCheckers.fileIsExecutable i3lock}
    ${utils.shellCheckers.fileIsExecutable pkill}
  '';

  minutes = 5;

  pkg = writeCheckedExecutable name checkPhase ''
    #! ${bash}
    exec <&-
    ${esc pkill} -x -U "$USER" -- ${esc (baseNameOf xautolock)} 2>/dev/null
    ${esc xautolock} -time ${esc minutes} -locker ${esc i3lock}' -c 111111' &>/dev/null &
  '';
in
{
  inherit name pkg checkPhase minutes;
}
