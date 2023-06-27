# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

args@{ pkgs, lib, ... }:

{
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = builtins.readFile ./xmonad.hs;

    extraPackages = hsPkgs: [
      hsPkgs.aeson
      hsPkgs.qm-interpolated-string
    ];

    # ghcArgs = [
    #   "-hidir /tmp" # Prevent from trying to save to Nix store
    #   "-odir /tmp" # Prevent from trying to save to Nix store
    #   "-i${somePkg}" # Extra package example
    # ];
  };
}
