args@{ ... }:
assert let k = "pkgs";  in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
assert let k = "utils"; in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
let
  pkgs = args.pkgs or (import <nixpkgs> {
    config = let k = "config"; in
      if builtins.hasAttr k args then {} else args."${k}".nixpkgs.config;
  });

  utils = args.utils or (import ../../nix-utils-pick.nix args).pkg;
  inherit (utils) esc nameOfModuleWrapDir;

  ghc = pkgs.haskellPackages.ghcWithPackages (p: [
    p.directory
    p.filepath
    p.typed-process
    p.unix
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
