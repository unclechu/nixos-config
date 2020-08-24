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
    rev = "4acba410efbe29729244da82a45d2c58f64a9462"; # ref "master", 24 August 2020
    sha256 = "1jxcmj6y5f88jb7445xq64rz8k7b70h9ifdj7r7hfl55d07rcj3d";
  };
in
import src { inherit pkgs; }
