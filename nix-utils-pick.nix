args@{ ... }:
assert let k = "pkgs"; in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
let
  pkgs = args.pkgs or (import <nixpkgs> {
    config = let k = "config"; in
      if builtins.hasAttr k args then {} else args."${k}".nixpkgs.config;
  });

  src = fetchGit {
    url = "https://github.com/unclechu/nix-utils.git";
    rev = "2ca60ebf9ff0d8a134caf678a90d9f56fcf85df8"; # 30 May 2020
    ref = "master";
  };
in
{
  inherit src;
  pkg = import src { inherit pkgs; };
}
