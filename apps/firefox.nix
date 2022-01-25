# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ callPackage
, lib
, firefox

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}
}:
let
  inherit (__nix-utils) wrapExecutable;
  binPath = "/bin/firefox";

  my-firefox = wrapExecutable "${firefox}${binPath}" {
    env = {
      MOZ_USE_XINPUT2 = 1; # support touchscreen scrolling
    };
  };
in
my-firefox // { executable = "${my-firefox}${binPath}"; }
