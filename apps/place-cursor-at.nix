args@{ ... }:
assert let k = "pkgs";   in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
assert let k = "utils";  in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
let
  pkgs = args.pkgs or (import <nixpkgs> {
    config = let k = "config"; in
      if builtins.hasAttr k args then {} else args."${k}".nixpkgs.config;
  });

  utils = args.utils or (import ../nix-utils-pick.nix args).pkg;
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
