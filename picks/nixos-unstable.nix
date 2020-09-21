args@{ ... }:
let
  config-k = "config";

  # ref "nixos-unstable", 19 September 2020
  commit = "2a35f664394b379e0c0785cc769ff6ccc791be39";

  unstable-nixpkgs-src = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "1ac01hyvniiwrwgqlvmx76dxc7aqg71nx3d05d0dc35lbyjq7acf";
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
