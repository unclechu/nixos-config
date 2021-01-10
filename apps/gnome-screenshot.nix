{ pkgs  ? import <nixpkgs> {}
, utils ? import (import ../nix/sources.nix).nix-utils { inherit pkgs; }
}:
let
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  dash = "${pkgs.dash}/bin/dash";
  gnome-screenshot = "${pkgs.gnome3.gnome-screenshot}/bin/${name}";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable dash}
    ${utils.shellCheckers.fileIsExecutable gnome-screenshot}
  '';
in
writeCheckedExecutable name checkPhase ''
  #! ${dash}
  DATE=$(date +'%Y-%m-%d %H-%M-%S') || exit
  ${esc gnome-screenshot} --file="$HOME/Pictures/Screenshots/$DATE.png" "$@"
''
