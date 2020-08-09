args@{ ... }:
let
  config-k = "config";
  commit = "4364ff933ebec0ef856912b182f4f9272aa7f98f"; # ref "nixos-20.03", 7 August 2020

  stable-nixpkgs-src = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "19ig1ywd2jq7qqzwpw6f1li90dq4kk3v0pbqgn6lzdabzf95bz6z";
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
