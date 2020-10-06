args@{ ... }:
let pkgs-k = "pkgs"; config-k = "config"; in
assert let k = pkgs-k;  in builtins.hasAttr k args -> builtins.isAttrs args.${k};
let
  pkgs = args.${pkgs-k} or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { ${k} = args.${k}.nixpkgs.${k}; } else {}
  ));

  src = pkgs.fetchFromGitHub {
    owner = "unclechu";
    repo = "xlib-keys-hack";
    rev = "74566012ff4d365f56633bdfdc0a69f717d96a97"; # ref "master", 6 October 2020
    sha256 = "14jb2b4sj777zd69ssvdk9s2yz78jx3jhy7fzbzkrqc0abldxj40";
  };
in
import src { inherit pkgs; }
