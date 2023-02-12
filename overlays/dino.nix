# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let
  sources = import ../nix/sources.nix;

  # 0.4.0 from nixpkgs “master”
  newDino = "${sources.nixpkgs-master}/pkgs/applications/networking/instant-messengers/dino/default.nix";
in

self: super:
{
  dino = super.callPackage newDino {
    inherit (super.gst_all_1) gstreamer gst-plugins-base gst-plugins-bad gst-vaapi;
    gst-plugins-good = super.gst_all_1.gst-plugins-good.override { gtkSupport = true; };
  };
}
