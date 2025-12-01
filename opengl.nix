# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ pkgs, ... }:
{
  hardware.graphics =
    let
      extras = [
        "mesa"
        "libva-vdpau-driver"
        "intel-vaapi-driver"
        "libvdpau-va-gl"
        "libva"
      ];
    in
      {
        enable = true;
        extraPackages = builtins.map (name: pkgs.${name}) extras;
        extraPackages32 = builtins.map (name: pkgs.pkgsi686Linux.${name}) extras;
      };
}
