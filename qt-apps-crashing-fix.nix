# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ pkgs, lib, ...  }:
let
  missing-gsettings-schemas-fix = builtins.readFile "${pkgs.stdenv.mkDerivation {
    name = "missing-gsettings-schemas-fix";
    dontUnpack = true; # Make it buildable without “src” attribute
    buildInputs = [ pkgs.gtk3 ];
    installPhase = '' printf %s "$GSETTINGS_SCHEMAS_PATH" > "$out" '';
  }}";
in
{
  # Fix for “No GSettings schemas are installed on the system” error
  # that causes Qt application to crash when opening file picker dialog window.
  environment.sessionVariables.XDG_DATA_DIRS = lib.mkAfter [ "${missing-gsettings-schemas-fix}" ];
}
