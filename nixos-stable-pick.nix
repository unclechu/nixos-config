{ config ? null, ... }:
let
  stable-nixpkgs-src = fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "48723f48ab92381f0afd50143f38e45cf3080405"; # 22 May 2020
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
