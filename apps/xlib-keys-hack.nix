args@{ ... }:
let pkgs-k = "pkgs"; utils-k = "utils"; config-k = "config"; in
assert let k = pkgs-k;  in builtins.hasAttr k args -> builtins.isAttrs args.${k};
assert let k = utils-k; in builtins.hasAttr k args -> builtins.isAttrs args.${k};
let
  pkgs = args.${pkgs-k} or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { ${k} = args.${k}.nixpkgs.${k}; } else {}
  ));

  utils = args.${utils-k} or (import ../picks/nix-utils.nix (
    pkgs.lib.filterAttrs (k: _: k == config-k) args // { inherit pkgs; }
  )).pkg;

  inherit (utils) nameOfModuleFile;

  src = pkgs.fetchFromGitHub {
    owner = "unclechu";
    repo = "xlib-keys-hack";
    rev = "a811fadf8f6a88fadf60b7678961049b2b0d59ec"; # ref "master", 13 May 2020
    sha256 = "1849jhaq3wc20pfakh12d0mqy8vh8ra8fzl12g1as49lv7ygws2n";
  };

  data-maybe-preserve-src = pkgs.fetchFromGitHub {
    owner = "unclechu";
    repo = "haskell-data-maybe-preserve";
    rev = "705e3e4e85661c6c3c34f43a408b111433abb27e"; # ref "master", 21 April 2018
    sha256 = "1m6vgkwp1c7n8yxfpsh6vgkvi8lj1svcq99js70gq54m7z9lffsb";
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
