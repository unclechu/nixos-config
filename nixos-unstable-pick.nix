args@{ ... }:
let
  config-k = "config";

  unstable-nixpkgs-src = fetchGit {
    url = "https://github.com/NixOS/nixpkgs.git";
    rev = "0a146054bdf6f70f66de4426f84c9358521be31e"; # 9 June 2020
    ref = "nixos-unstable";
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
