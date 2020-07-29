args@{ ... }:
let
  config-k = "config";
  commit = "28fce082c8ca1a8fb3dfac5c938829e51fb314c8"; # ref "nixos-unstable", 26 July 2020

  unstable-nixpkgs-src = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "1pzmqgby1g9ypdn6wgxmbhp6hr55dhhrccn67knrpy93vib9wf8r";
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
