args@{ ... }:
let pkgs-k = "pkgs"; utils-k = "utils"; config-k = "config"; in
assert let k = pkgs-k;  in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
assert let k = utils-k; in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
let
  pkgs = args."${pkgs-k}" or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { "${k}" = args."${k}".nixpkgs."${k}"; } else {}
  ));

  utils = args."${utils-k}" or (import ../nix-utils-pick.nix args).pkg;
  inherit (utils) nameOfModuleFile;

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
