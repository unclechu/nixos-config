{ config ? null, ... }:
let
  unstable-nixpkgs-src = fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "8ba41a1e14961fe43523f29b8b39acb569b70e72"; # 14 May 2020
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
