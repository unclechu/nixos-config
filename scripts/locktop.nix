args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, dzen-box
, wenzels-keyboard
, ...
}:
let
  utils = import ../utils args;
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;

  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.bash;

  bash = "${pkgs.bash}/bin/bash";
  dzen-box-exe = "${dzen-box}/bin/dzen-box";
  wenzels-keyboard-exe = "${wenzels-keyboard}/bin/wenzels-keyboard";
  xautolock = "${pkgs.xautolock}/bin/xautolock";
  i3lock = "${pkgs.i3lock}/bin/i3lock";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable bash}
    ${utils.shellCheckers.fileIsExecutable dzen-box-exe}
    ${utils.shellCheckers.fileIsExecutable xautolock}
  '';

  pkg = writeCheckedExecutable name checkPhase ''
    #! ${bash}
    ${esc dzen-box-exe} LOCK orangered
    ${esc wenzels-keyboard-exe} --no-xlib-hack
    if ! ${esc xautolock} -locknow; then ${esc i3lock} -c 111111; fi
  '';
in
{
  inherit name pkg checkPhase;
}
