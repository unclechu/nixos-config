let sources = import ../nix/sources.nix; in
{ callPackage
, pkgs # TODO remove when ‘wenzels-keyboard’ get refactored
, lib
, bash
, xautolock
, i3lock
, jack2

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}
, __dzen-box ? callPackage ./dzen-box { inherit __nix-utils; }

, wenzels-keyboard ? import ./wenzels-keyboard { inherit pkgs; }
}:
assert lib.isDerivation __dzen-box;
assert lib.isDerivation wenzels-keyboard;
let
  inherit (__nix-utils) esc writeCheckedExecutable nameOfModuleFile shellCheckers;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.bash;
  bash-exe = "${bash}/bin/bash";
  dzen-box = "${__dzen-box}/bin/dzen-box";
  wenzels-keyboard-exe = "${wenzels-keyboard}/bin/${wenzels-keyboard.name}";
  xautolock-exe = "${xautolock}/bin/xautolock";
  i3lock-exe = "${i3lock}/bin/i3lock";
  jack_control = "${jack2}/bin/jack_control";

  checkPhase = ''
    ${shellCheckers.fileIsExecutable bash-exe}
    ${shellCheckers.fileIsExecutable dzen-box}
    ${shellCheckers.fileIsExecutable wenzels-keyboard-exe}
    ${shellCheckers.fileIsExecutable xautolock-exe}
    ${shellCheckers.fileIsExecutable i3lock-exe}
    ${shellCheckers.fileIsExecutable jack_control}
  '';
in
writeCheckedExecutable name checkPhase ''
  #! ${bash-exe}
  ${esc dzen-box} LOCK orangered
  ${esc wenzels-keyboard-exe} --no-xlib-hack
  ${esc jack_control} stop
  if [[ -x ~/.screenlayout/only-laptop.sh ]]; then ~/.screenlayout/only-laptop.sh; fi
  if ! ${esc xautolock-exe} -locknow; then ${esc i3lock-exe} -c 111111; fi
''
