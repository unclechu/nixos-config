let sources = import ../nix/sources.nix; in
{ pkgs
, nix-utils ? pkgs.callPackage sources.nix-utils {}
}:
let
  inherit (nix-utils) esc writeCheckedExecutable nameOfModuleFile;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  dash = "${pkgs.dash}/bin/dash";
  gnome-screenshot = "${pkgs.gnome3.gnome-screenshot}/bin/${name}";

  checkPhase = ''
    ${nix-utils.shellCheckers.fileIsExecutable dash}
    ${nix-utils.shellCheckers.fileIsExecutable gnome-screenshot}
  '';
in
writeCheckedExecutable name checkPhase ''
  #! ${dash}
  DATE=$(date +'%Y-%m-%d %H-%M-%S') || exit
  ${esc gnome-screenshot} --file="$HOME/Pictures/Screenshots/$DATE.png" "$@"
''
