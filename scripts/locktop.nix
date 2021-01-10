{ pkgs             ? import <nixpkgs> {}
, utils            ? import (import ../nix/sources.nix).nix-utils { inherit pkgs; }
, dzen-box         ? import ./dzen-box { inherit pkgs; }
, wenzels-keyboard ? import ./wenzels-keyboard { inherit pkgs; }
}:
assert pkgs.lib.isDerivation dzen-box;
assert pkgs.lib.isDerivation wenzels-keyboard;
let
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.bash;
  bash = "${pkgs.bash}/bin/bash";
  dzen-box-exe = "${dzen-box}/bin/dzen-box";
  wenzels-keyboard-exe = "${wenzels-keyboard}/bin/${wenzels-keyboard.name}";
  xautolock = "${pkgs.xautolock}/bin/xautolock";
  i3lock = "${pkgs.i3lock}/bin/i3lock";
  jack_control = "${pkgs.jack2}/bin/jack_control";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable bash}
    ${utils.shellCheckers.fileIsExecutable dzen-box-exe}
    ${utils.shellCheckers.fileIsExecutable wenzels-keyboard-exe}
    ${utils.shellCheckers.fileIsExecutable xautolock}
    ${utils.shellCheckers.fileIsExecutable i3lock}
    ${utils.shellCheckers.fileIsExecutable jack_control}
  '';
in
writeCheckedExecutable name checkPhase ''
  #! ${bash}
  ${esc dzen-box-exe} LOCK orangered
  ${esc wenzels-keyboard-exe} --no-xlib-hack
  ${esc jack_control} stop
  if [[ -x ~/.screenlayout/only-laptop.sh ]]; then ~/.screenlayout/only-laptop.sh; fi
  if ! ${esc xautolock} -locknow; then ${esc i3lock} -c 111111; fi
''
