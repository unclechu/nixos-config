{ config ? null, ... }:
let
  unstable-nixpkgs-src = fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "0f5ce2fac0c726036ca69a5524c59a49e2973dd4"; # 19 May 2020
    ref = "nixos-unstable";
  };

  unstable-nixpkgs = import unstable-nixpkgs-src {
    config = if config == null then {} else config.nixpkgs.config;
  };
in
{
  src = unstable-nixpkgs-src;
  pkgs = unstable-nixpkgs;
}
