# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ callPackage
, writeTextFile
, dash
, gnome-screenshot

# Overridable dependencies
, executable-dependencies ? callPackage ../utils/executable-dependencies.nix {}
}:
let
  e = executable-dependencies {
    dash = dash;
    gnome-screenshot = gnome-screenshot;
  };
in
writeTextFile rec {
  name = "gnome-screenshot";
  executable = true;
  destination = "/bin/${name}";
  inherit (e) checkPhase;
  text = ''
    #! ${e.b.dash}
    DATE=$(date +'%Y-%m-%d %H-%M-%S') || exit
    ${e.s.gnome-screenshot} --file="$HOME/Pictures/Screenshots/$DATE.png" "$@"
  '';
}
