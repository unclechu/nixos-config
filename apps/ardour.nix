# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../nix/sources.nix; in

{ lib, fetchgit, ardour }:

# Using newer Ardour (7.4 instead of 7.1)
ardour.overrideAttrs (old: rec {
  # “postInstall” contains links to 7 major version of Ardour
  version = assert lib.versions.major old.version == "7"; "7.4";

  src = fetchgit {
    url = "git://git.ardour.org/ardour/ardour.git";
    rev = version;
    hash = "sha256-CUGhJi3ji0F6v41Y08sQvo7oKITOJ96ojdJL+FyCxmw=";
  };

  postPatch = builtins.replaceStrings [old.version] [version] old.postPatch;
})
