args@{ ... }:
let
  unstable-nixpkgs-src = fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "0f5ce2fac0c726036ca69a5524c59a49e2973dd4"; # 19 May 2020
    ref = "nixos-unstable";
  };

  unstable-nixpkgs = import unstable-nixpkgs-src {
    config = let k = "config"; in
      if builtins.hasAttr k args then {} else args."${k}".nixpkgs.config;
  };
in
{
  src = unstable-nixpkgs-src;
  pkgs = unstable-nixpkgs;
}
