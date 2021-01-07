args@{ ... }:
let pkgs-k = "pkgs"; config-k = "config"; in
assert let k = pkgs-k; in builtins.hasAttr k args -> builtins.isAttrs args.${k};
let
  pkgs = args.${pkgs-k} or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { ${k} = args.${k}.nixpkgs.${k}; } else {}
  ));

  src = pkgs.fetchFromGitHub {
    owner = "unclechu";
    repo = "nix-utils";
    rev = "9d5fabedd99843b86d2bdee40f6b0447fabb20cb"; # ref “master”, 07 January 2021
    sha256 = "0ry8fg94fz9gljcy0m4yi331wzy2zrh80414dfp4xhbzdiyjwa37";
  };
in
{
  inherit src;
  pkg = import src { inherit pkgs; };
}
