# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../nix/sources.nix; in

{ lib, fetchgit, ardour }:

# Using newer Ardour (7.5 instead of 7.3)
ardour.overrideAttrs (old: rec {
  # “postInstall” contains links to 7 major version of Ardour
  version = assert lib.versions.major old.version == "7"; "7.5";

  src = fetchgit {
    url = "git://git.ardour.org/ardour/ardour.git";
    rev = version;
    hash = "sha256-cmYt6fGYuuVs6YhAXaO9AG6TrYLDVUaE1/iC67rt76I=";
  };

  postPatch = builtins.replaceStrings [old.version] [version] old.postPatch;
})
