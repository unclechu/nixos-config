args@{ ... }:
let
  config-k = "config";
  commit = "feff2fa6659799fe7439038b3eba453d62a16e69"; # ref "nixos-20.03", 26 August 2020

  stable-nixpkgs-src = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "0vlnrwlxl6xf6b8rmiy7as2lhi015nklyj2xdiy3ly8xznq69ll9";
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
