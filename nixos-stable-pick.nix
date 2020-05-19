{ config ? null, ... }:
let
  stable-nixpkgs-src = fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "a4f8cec54dcf77b322947117e9b9a0c444bcb17d"; # 12 May 2020
    ref = "nixos-20.03";
  };

  stable-nixpkgs = import stable-nixpkgs-src {
    config = if config == null then {} else config.nixpkgs.config;
  };
in
{
  src = stable-nixpkgs-src;
  pkgs = stable-nixpkgs;
}
