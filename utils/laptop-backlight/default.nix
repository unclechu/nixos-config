args@{ ... }:
let pkgs-k = "pkgs"; utils-k = "utils"; config-k = "config"; in
assert let k = pkgs-k;  in builtins.hasAttr k args -> builtins.isAttrs args.${k};
assert let k = utils-k; in builtins.hasAttr k args -> builtins.isAttrs args.${k};
let
  pkgs = args.${pkgs-k} or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { ${k} = args.${k}.nixpkgs.${k}; } else {}
  ));

  sources = import ../../nix/sources.nix;
  utils = args.${utils-k} or (import sources.nix-utils { inherit pkgs; });

  inherit (utils) esc nameOfModuleWrapDir;

  ghc = pkgs.haskellPackages.ghcWithPackages (p: [
    p.attoparsec
    p.filepath
  ]);

  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.hs;
  srcFile = pkgs.writeText "${name}.hs" src;

  pkg = pkgs.runCommand name {} ''
    set -Eeuo pipefail
    ${esc ghc}/bin/ghc -Wall -O2 -o "$out" ${esc srcFile}
    ${esc pkgs.coreutils}/bin/chmod +x -- "$out"
  '';
in
{
  inherit name pkg ghc;
}
