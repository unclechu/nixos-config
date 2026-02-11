# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ callPackage
, lib
, dash
, xorg # Just for ‘xinput’

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}
, __pointers ? callPackage ./pointers.nix {}
}:
let
  inherit (__nix-utils) esc writeCheckedExecutable nameOfModuleFile shellCheckers;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  pointers = lib.filterAttrs (n: v: lib.isDerivation v) __pointers;

  dependencies = {
    dash = dash;
    xinput = xorg.xinput;
  } // pointers;

  executables = builtins.mapAttrs (n: v: "${v}/bin/${n}") dependencies;

  checkPhase =
    builtins.concatStringsSep "\n"
      (map shellCheckers.fileIsExecutable (builtins.attrValues executables));
in
writeCheckedExecutable name checkPhase ''
  #! ${executables.dash}
  exec <&- 1>/dev/null 2>/dev/null # silent

  # all pointer setup scripts (mice)
  ${
    builtins.concatStringsSep "\n"
      (map (name: "${esc (executables.${name})} &") (builtins.attrNames pointers))
  }

  # laptop touchscreen
  ${esc executables.xinput} --map-to-output 'ELAN25B6:00 04F3:0732' eDP1

  # prevent returning exit status of the latest command (it’s okay to fail)
  exit 0
''
