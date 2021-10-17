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
, __pointers ? callPackage ./pointers.nix {}

# Build options
, __srcScript ? ./main.bash
}:
let
  inherit (__nix-utils) esc writeCheckedExecutable nameOfModuleFile shellCheckers;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile __srcScript;

  pointers = lib.filterAttrs (n: v: lib.isDerivation v) __pointers;

  dependencies = {
    bash = bash;
    xinput = xlibs.xinput;
    ${lib.getName __wenzels-keyboard} = __wenzels-keyboard;
  } // pointers;

  executables = builtins.mapAttrs (n: v: "${v}/bin/${n}") dependencies;

  checkPhase =
    builtins.concatStringsSep "\n"
      (map shellCheckers.fileIsExecutable (builtins.attrValues executables));
in
writeCheckedExecutable name checkPhase ''
  #! ${executables.bash}
  exec <&- &>/dev/null

  # keyboard
  ${esc executables.${lib.getName __wenzels-keyboard}} &

  # mouse
  ${
    builtins.concatStringsSep "\n"
      (map (name: "${esc (executables.${name})} &") (builtins.attrNames pointers))
  }

  # laptop touchscreen
  ${esc executables.xinput} --map-to-output 'ELAN25B6:00 04F3:0732' eDP1

  exit 0 # prevent returning exit status of the latest command
''
