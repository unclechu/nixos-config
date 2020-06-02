args@{ ... }:
let
  stable-nixpkgs-src = fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "48723f48ab92381f0afd50143f38e45cf3080405"; # 22 May 2020
    ref = "nixos-20.03";
  };

  stable-nixpkgs = import stable-nixpkgs-src {
    config = let k = "config"; in
      if builtins.hasAttr k args then {} else args."${k}".nixpkgs.config;
  };
in
{
  src = stable-nixpkgs-src;
  pkgs = stable-nixpkgs;
}
