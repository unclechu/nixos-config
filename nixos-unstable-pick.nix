args@{ ... }:
let
  config-k = "config";
  commit = "b50ef9afa11b384c72f7123ca4760ebc6d199fe8"; # ref "nixos-unstable", 8 August 2020

  unstable-nixpkgs-src = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "0b6xjysxydiax765sia3dpnc3xi648aq4zjlxpiqzsh3hpsq0ch8";
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
