let sources = import ../nix/sources.nix; in
{ callPackage
, dash
, gnome3

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}
}:
let
  inherit (__nix-utils) esc writeCheckedExecutable nameOfModuleFile shellCheckers;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  dash-exe = "${dash}/bin/dash";
  gnome-screenshot = "${gnome3.gnome-screenshot}/bin/${name}";

  checkPhase = ''
    ${shellCheckers.fileIsExecutable dash-exe}
    ${shellCheckers.fileIsExecutable gnome-screenshot}
  '';
in
writeCheckedExecutable name checkPhase ''
  #! ${dash-exe}
  DATE=$(date +'%Y-%m-%d %H-%M-%S') || exit
  ${esc gnome-screenshot} --file="$HOME/Pictures/Screenshots/$DATE.png" "$@"
''
