let sources = import ../nix/sources.nix; in
{ pkgs
, nix-utils ? pkgs.callPackage sources.nix-utils {}

, minutes ? 5
}:
let
  inherit (nix-utils) esc writeCheckedExecutable nameOfModuleFile;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  bash = "${pkgs.bash}/bin/bash";
  xautolock = "${pkgs.xautolock}/bin/xautolock";
  i3lock = "${pkgs.i3lock}/bin/i3lock";
  pkill = "${pkgs.procps}/bin/pkill";

  checkPhase = ''
    ${nix-utils.shellCheckers.fileIsExecutable bash}
    ${nix-utils.shellCheckers.fileIsExecutable xautolock}
    ${nix-utils.shellCheckers.fileIsExecutable i3lock}
    ${nix-utils.shellCheckers.fileIsExecutable pkill}
  '';
in
writeCheckedExecutable name checkPhase ''
  #! ${bash}
  exec <&-
  ${esc pkill} -x -U "$USER" -- ${esc (baseNameOf xautolock)} 2>/dev/null
  ${esc xautolock} -time ${esc minutes} -locker ${esc i3lock}' -c 111111' &>/dev/null &
''
