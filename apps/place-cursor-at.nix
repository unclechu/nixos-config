args@{ ... }:
let pkgs-k = "pkgs"; utils-k = "utils"; config-k = "config"; in
assert let k = pkgs-k;   in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
assert let k = utils-k;  in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
let
  pkgs = args."${pkgs-k}" or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { "${k}" = args."${k}".nixpkgs."${k}"; } else {}
  ));

  utils = args."${utils-k}" or (import ../nix-utils-pick.nix args).pkg;
  inherit (utils) nameOfModuleFile;

  src = fetchGit {
    url = "https://github.com/unclechu/place-cursor-at.git";
    rev = "1a5a11550e03fb679e43940e67a9e04d6aeb515d"; # 8 March 2020
    ref = "master";
  };

  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  pkg = pkgs.haskellPackages.callCabal2nix name src {};
in
{
  inherit name src;
  haskell-pkg = pkg;
  pkg = pkgs.haskell.lib.justStaticExecutables pkg;
}
