args@{ ... }:
let
  config-k = "config";

  # ref "nixos-unstable", 12 September 2020
  commit = "e0759a49733dfc3aa225b8a7423c00da6e1ecb67";

  unstable-nixpkgs-src = fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${commit}.tar.gz";
    sha256 = "1lnaifrbdmvbmz25404z7xpfwaagscs1i80805fyrrs1g27h21qb";
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
