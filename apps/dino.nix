# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../nix/sources.nix; in

{ fetchFromGitHub, dino, gtk4, libadwaita }:

dino.overrideAttrs (old: {
  version = sources.dino.branch;
  src = sources.dino;

  buildInputs = old.buildInputs ++ [
    gtk4 # The newer version depends on GTK4 instead of GTK3
    libadwaita
  ];
})
