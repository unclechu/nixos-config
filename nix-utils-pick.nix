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
    rev = "377b3b35a50d482b9968d8d19bcb98cc4c37d6bd"; # ref "master", 9 July 2020
    sha256 = "1cikgl25a0x497v3hc7yxri2jbdm6cn7ld891ak7fhxrdb6bmlpl";
  };
in
{
  inherit src;
  pkg = import src { inherit pkgs; };
}
