# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{ pkgs ? import <nixpkgs> {}

# Pick sources from the Polybar locally cloned repo.
# See “./polybar.nix” for more details.
, useLocalSources ? false
}:

pkgs.callPackage ./polybar.nix { inherit useLocalSources; }
