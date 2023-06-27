# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
#
# nix-shell setup that is useful for development of the XMonad config.
#
# It provides GHC with dependencies and HLS for LSP editor integration.

{ pkgs ? import <nixpkgs> {}
, lib ? pkgs.lib
, haskellPackages ? pkgs.haskellPackages
, inNixShell ? false # Set to “true” automatically when used by nix-shell
}:

let
  xmonadConfig = import ./. { inherit pkgs lib; };
  inherit (xmonadConfig.services.xserver.windowManager) xmonad;
  hsPkgsFn = xmonad.extraPackages;

  ghc = haskellPackages.ghcWithPackages (
    p: [
      p.xmonad
    ] ++ lib.optionals xmonad.enableContribAndExtras [
      p.xmonad-contrib
      p.xmonad-extras
    ] ++ hsPkgsFn p
  );

  shell = pkgs.mkShell {
    buildInputs = [
      (pkgs.haskell-language-server.override {
        inherit haskellPackages;

        supportedGhcVersions = [
          (builtins.replaceStrings ["."] [""] ghc.version)
        ];
      })

      ghc
    ];
  };
in

(if inNixShell then shell else {}) // {
  inherit ghc hsPkgsFn;
}
