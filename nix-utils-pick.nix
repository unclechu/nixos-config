args@{ ... }:
let pkgs-k = "pkgs"; config-k = "config"; in
assert let k = pkgs-k; in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
let
  pkgs = args."${pkgs-k}" or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { "${k}" = args."${k}".nixpkgs."${k}"; } else {}
  ));

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
