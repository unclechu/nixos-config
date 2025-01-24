# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# This is a temporary fix for the issue https://github.com/NixOS/nixpkgs/issues/375865
#
# This function is supposed to be applied against `boot.kernelPackages` like so:
#
#    boot.kernelPackages =
#      (import ./fix-cpupower-build-failure.nix) pkgs pkgs.linuxPackages_latest;
#
pkgs: linuxPackages:
  linuxPackages.extend (lpFinal: lpPrev: {
    cpupower = lpPrev.cpupower.overrideAttrs (old: {
      nativeBuildInputs = (old.nativeBuildInputs or []) ++ [ pkgs.which ];
      makeFlags = (old.makeFlags or []) ++ [ "INSTALL_NO_TRANSLATIONS=1" ];
    });
  })
