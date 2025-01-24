# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ callPackage
, dash
, gnome-screenshot

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}
}:
let
  inherit (__nix-utils) esc writeCheckedExecutable nameOfModuleFile shellCheckers;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  dash-exe = "${dash}/bin/dash";
  gnome-screenshot-exe = "${gnome-screenshot}/bin/${name}";

  checkPhase = ''
    ${shellCheckers.fileIsExecutable dash-exe}
    ${shellCheckers.fileIsExecutable gnome-screenshot-exe}
  '';
in
writeCheckedExecutable name checkPhase ''
  #! ${dash-exe}
  DATE=$(date +'%Y-%m-%d %H-%M-%S') || exit
  ${esc gnome-screenshot-exe} --file="$HOME/Pictures/Screenshots/$DATE.png" "$@"
''
