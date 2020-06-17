args@{ ... }:
let
  config-k = "config";

  stable-nixpkgs-src = fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "db31e48c5c8d99dcaf4e5883a96181f6ac4ad6f6"; # 11 June 2020
    ref = "nixos-20.03";
  };

  stable-nixpkgs = import stable-nixpkgs-src (
    let k = config-k; in
    if builtins.hasAttr k args then { ${k} = args.${k}.nixpkgs.${k}; } else {}
  );
in
{
  src = stable-nixpkgs-src;
  pkgs = stable-nixpkgs;
}
