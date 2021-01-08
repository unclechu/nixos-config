args@{ ... }:
let pkgs-k = "pkgs"; utils-k = "utils"; config-k = "config"; in
assert let k = pkgs-k;  in builtins.hasAttr k args -> builtins.isAttrs args.${k};
assert let k = utils-k; in builtins.hasAttr k args -> builtins.isAttrs args.${k};
let
  pkgs = args.${pkgs-k} or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { ${k} = args.${k}.nixpkgs.${k}; } else {}
  ));

  sources = import ../nix/sources.nix;
  utils = args.${utils-k} or (import sources.nix-utils { inherit pkgs; });
  inherit (utils) esc wrapExecutable nameOfModuleFile;

  src = pkgs.fetchFromGitHub {
    owner = "unclechu";
    repo = "unclechu-i3-status";
    rev = "fe8744725175f5304f9dd70f7aeb91cda1f99a3a"; # ref "master", 22 November 2020
    sha256 = "1qssfqc7sxm12j0m08nbgknxzxs3q68wdyljqjlwyb4bfhpqrcqm";
  };

  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  pkg = pkgs.haskellPackages.callCabal2nix name src {};
  pkg-exe = "${pkgs.haskell.lib.justStaticExecutables pkg}/bin/${name}";

  dzen2 = "${pkgs.dzen2}/bin/dzen2";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable dzen2}
    ${utils.shellCheckers.fileIsExecutable pkg-exe}
  '';

  wrapper = wrapExecutable pkg-exe { deps = [ pkgs.dzen2 ]; inherit checkPhase; };
in
{
  inherit name src;
  haskell-pkg = pkg;
  pkg = wrapper;
}
