let sources = import ../nix/sources.nix; in
{ callPackage
, bash
, xlibs # Just for ‘xinput’

, pkgs # TODO Remove when dependencies are refactored

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}
, __wenzels-keyboard ? import ./wenzels-keyboard { inherit pkgs; }

# Build options
, pointers ? [
    (callPackage ./pointer-dell-latitude-laptop-dot {})
    (callPackage ./pointer-dell-latitude-laptop-touchpad {})
    (callPackage ./pointer-logitech-wireless-ambidextrous-small-mouse {})
    (callPackage ./pointer-logitech-wireless-t650-touchpad {})
    (callPackage ./pointer-razor-wired-ambidextrous-mouse {})
  ]
}:
let
  inherit (__nix-utils) esc writeCheckedExecutable nameOfModuleFile shellCheckers;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.bash;
  bash-exe = "${bash}/bin/bash";
  xinput = "${xlibs.xinput}/bin/xinput";
  exe = drv: "${drv}/bin/${drv.name}";
  wenzels-keyboard = exe __wenzels-keyboard;

  checkPhase = ''
    ${shellCheckers.fileIsExecutable bash-exe}
    ${shellCheckers.fileIsExecutable xinput}
    ${shellCheckers.fileIsExecutable wenzels-keyboard}
    ${builtins.concatStringsSep "\n" (map (x: shellCheckers.fileIsExecutable (exe x)) pointers)}
  '';
in
writeCheckedExecutable name checkPhase ''
  #! ${bash-exe}
  exec <&- &>/dev/null

  # keyboard
  ${esc wenzels-keyboard} &

  # mouse
  ${builtins.concatStringsSep "\n" (map (x: "${esc (exe x)} &") pointers)}

  # laptop touchscreen
  ${esc xinput} --map-to-output 'ELAN25B6:00 04F3:0732' eDP1

  exit 0 # prevent returning exit status of the latest command
''
