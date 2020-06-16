args@{ ... }:
let pkgs-k = "pkgs"; config-k = "config"; in
assert let k = pkgs-k; in builtins.hasAttr k args -> builtins.isAttrs args.${k};
let
  pkgs = args.${pkgs-k} or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { ${k} = args.${k}.nixpkgs.${k}; } else {}
  ));

  src = fetchGit {
    url = "https://github.com/unclechu/nix-utils.git";
    rev = "8fbdc3c63404b58275b468aaf508d5c7c198fc4b"; # 17 June 2020
    ref = "master";
  };
in
{
  inherit src;
  pkg = import src { inherit pkgs; };
}
