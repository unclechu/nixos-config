# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
#
# A system profile with dGPU enabled globally (DRI_PRIME=1).
# For machines with iGPU and dGPU (typically laptops).
#
args@{ config, pkgs, lib, ... }:

let
  inherit (import ./constants.nix) systemProfile;
  hostName = config.networking.hostName or null;
  wenzel-silver-laptop = import hardware/wenzel-silver-laptop.nix { inherit config pkgs lib; };
in

{
  imports = [
    ./configuration.nix
  ];

  nixpkgs.overlays = import ./overlays ++ [
    # A hack to make system profile name available in all of the modules.
    # It’s available as “pkgs.systemProfile” but only inside this NixOS configuration
    # (not available in <nixpkgs> channel).
    (self: super: { systemProfile = systemProfile.graphics; })
  ];

  environment.variables.DRI_PRIME = lib.mkForce (
    # Only these machines are supposed to be configured for iGPU + dGPU.
    assert builtins.elem hostName [
      wenzel-silver-laptop.networking.hostName
    ];
    "1"
  );
}
