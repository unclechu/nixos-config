# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ callPackage
, lib
, bash
, xlibs # Just for ‘xinput’

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}
, __wenzels-keyboard ? callPackage ./wenzels-keyboard { inherit __nix-utils; }

# Build options
, __srcScript ? ./main.bash
, pointers ? [
    (callPackage ./pointer-dell-latitude-laptop-dot                   { inherit __nix-utils; })
    (callPackage ./pointer-dell-latitude-laptop-touchpad              { inherit __nix-utils; })
    (callPackage ./pointer-logitech-wireless-ambidextrous-small-mouse { inherit __nix-utils; })
    (callPackage ./pointer-logitech-wireless-t650-touchpad            { inherit __nix-utils; })
    (callPackage ./pointer-razor-wired-ambidextrous-mouse             { inherit __nix-utils; })
    (callPackage ./pointer-logitech-g-pro.nix                         { inherit __nix-utils; })
  ]
}:
let
  inherit (__nix-utils) esc writeCheckedExecutable nameOfModuleFile shellCheckers;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile __srcScript;

  dependencies = {
    bash = bash;
    xinput = xlibs.xinput;
    ${__wenzels-keyboard.name} = __wenzels-keyboard;
  } // (
    lib.attrsets.listToAttrs (map (x: { name = x.name; value = x; }) pointers)
  );

  executables = builtins.mapAttrs (n: v: "${dependencies.${n}}/bin/${n}") dependencies;

  checkPhase =
    builtins.concatStringsSep "\n"
      (map shellCheckers.fileIsExecutable (builtins.attrValues executables));
in
writeCheckedExecutable name checkPhase ''
  #! ${executables.bash}
  exec <&- &>/dev/null

  # keyboard
  ${esc executables.${__wenzels-keyboard.name}} &

  # mouse
  ${builtins.concatStringsSep "\n" (map (x: "${esc (executables.${x.name})} &") pointers)}

  # laptop touchscreen
  ${esc executables.xinput} --map-to-output 'ELAN25B6:00 04F3:0732' eDP1

  exit 0 # prevent returning exit status of the latest command
''
