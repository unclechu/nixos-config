args@{ ... }:
let
  config-k = "config";

  # ref "nixos-20.09", 13 September 2020
  commit = "e0508c81809b12013bce95562d556b1e672e3541";

  stable-nixpkgs-src = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "0y4p9rbyc5zmzfcpjmhbbhpxrp808wcnp2100fvgc0nyvkyl112b";
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
