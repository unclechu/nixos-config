args@{ ... }:
let
  config-k = "config";

  # ref "nixos-20.09", 18 September 2020
  commit = "be92e21e2519143f8c6ea70fa8052deff5126ce5";

  stable-nixpkgs-src = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "007cnvivw5y1505frq7dlwwmgzkxyb9dq3jlj8sw0r3zm00mzpr0";
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
