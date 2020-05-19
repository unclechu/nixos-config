args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, ...
}:
let
  inherit (import ../utils args) nameOfModuleFile;

  src = fetchGit {
    url = "https://github.com/unclechu/xlib-keys-hack.git";
    rev = "a811fadf8f6a88fadf60b7678961049b2b0d59ec"; # 13 May 2020
    ref = "master";
  };

  data-maybe-preserve-src = fetchGit {
    url = "https://github.com/unclechu/haskell-data-maybe-preserve.git";
    rev = "705e3e4e85661c6c3c34f43a408b111433abb27e"; # 21 April 2018
    ref = "master";
  };

  data-maybe-preserve = hs.callCabal2nix "data-maybe-preserve" data-maybe-preserve-src {};

  hs = pkgs.haskellPackages.extend (self: super: {
    inherit data-maybe-preserve;
  });

  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  pkg = hs.callCabal2nix name src {};
in
{
  inherit name src data-maybe-preserve-src;
  haskell-pkg = pkg;
  pkg = pkgs.haskell.lib.justStaticExecutables pkg;
  haskellPackages = hs;
}
