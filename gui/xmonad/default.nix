# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{ pkgs
, lib

# For patching XMonad core itself you can clone https://github.com/xmonad/xmonad repo locally
# (see “localXmonadPath”) and set this flag to “true”.
, useLocalXmonad ? false

, localXmonadPath ? ./xmonad-src

, ...
}:

let
  inherit (pkgs.haskell.lib) dontCheck;
in

{
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = false; # Added in “extraPackages” instead
    config = builtins.readFile ./xmonad.hs;

    extraPackages = hsPkgs:
      let
        # For development & testing
        localXmonad = hsPkgs.callCabal2nix "xmonad" (lib.cleanSource localXmonadPath) {};

        newHsPkgs = if ! useLocalXmonad then hsPkgs else hsPkgs.extend (self: super: {
          # “dontCheck” is just to make XMonad builds faster by skipping testing phase
          xmonad = dontCheck localXmonad;
          xmonad-contrib = dontCheck super.xmonad-contrib;
        });
      in
      [
        newHsPkgs.aeson
        newHsPkgs.qm-interpolated-string
        newHsPkgs.xmonad
        newHsPkgs.xmonad-contrib
      ];

    # ghcArgs = [
    #   "-hidir /tmp" # Prevent from trying to save to Nix store
    #   "-odir /tmp" # Prevent from trying to save to Nix store
    #   "-i${somePkg}" # Extra package example
    # ];
  };
}
