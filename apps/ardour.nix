# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../nix/sources.nix; in

{ lib, fetchgit, ardour }:

# Using newer Ardour (7.3 instead of 7.1)
ardour.overrideAttrs (old: rec {
  # “postInstall” contains links to 7 major version of Ardour
  version = assert lib.versions.major old.version == "7"; "7.3";

  src = fetchgit {
    url = "git://git.ardour.org/ardour/ardour.git";
    rev = version;
    hash = "sha256-fDZGmKQ6qgENkq8NY/J67Jym+IXoOYs8DT4xyPXLcC4=";
  };

  postPatch = builtins.replaceStrings [old.version] [version] old.postPatch;
})
