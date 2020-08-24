args@{ ... }:
let
  config-k = "config";
  commit = "c59ea8b8a0e7f927e7291c14ea6cd1bd3a16ff38"; # ref "nixos-unstable", 20 August 2020

  unstable-nixpkgs-src = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "1ak7jqx94fjhc68xh1lh35kh3w3ndbadprrb762qgvcfb8351x8v";
  };

  unstable-nixpkgs = import unstable-nixpkgs-src (
    let k = config-k; in
    if builtins.hasAttr k args then { ${k} = args.${k}.nixpkgs.${k}; } else {}
  );
in
{
  src = unstable-nixpkgs-src;
  pkgs = unstable-nixpkgs;
}
