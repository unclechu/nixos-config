let sources = import ../nix/sources.nix; in
{ pkgs
, nix-utils ? pkgs.callPackage sources.nix-utils {}

, wenzels-keyboard ? import ./wenzels-keyboard { inherit pkgs; }

, pointers ? [
    (import ./pointer-dell-latitude-laptop-dot { inherit pkgs; })
    (import ./pointer-dell-latitude-laptop-touchpad { inherit pkgs; })
    (import ./pointer-logitech-wireless-ambidextrous-small-mouse { inherit pkgs; })
    (import ./pointer-logitech-wireless-t650-touchpad { inherit pkgs; })
    (import ./pointer-razor-wired-ambidextrous-mouse { inherit pkgs; })
  ]
}:
let
  inherit (nix-utils) esc writeCheckedExecutable nameOfModuleFile;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.bash;
  bash = "${pkgs.bash}/bin/bash";
  xinput = "${pkgs.xlibs.xinput}/bin/xinput";
  exe = drv: "${drv}/bin/${drv.name}";
  wenzels-keyboard-exe = exe wenzels-keyboard;

  checkPhase = ''
    ${nix-utils.shellCheckers.fileIsExecutable bash}
    ${nix-utils.shellCheckers.fileIsExecutable xinput}
    ${nix-utils.shellCheckers.fileIsExecutable wenzels-keyboard-exe}
    ${
      builtins.concatStringsSep "\n"
        (map (x: nix-utils.shellCheckers.fileIsExecutable (exe x)) pointers)
    }
  '';
in
writeCheckedExecutable name checkPhase ''
  #! ${bash}
  exec <&- &>/dev/null

  # keyboard
  ${esc wenzels-keyboard-exe} &

  # mouse
  ${builtins.concatStringsSep "\n" (map (x: "${esc (exe x)} &") pointers)}

  # laptop touchscreen
  ${esc xinput} --map-to-output 'ELAN25B6:00 04F3:0732' eDP1

  exit 0 # prevent returning exit status of the latest command
''
