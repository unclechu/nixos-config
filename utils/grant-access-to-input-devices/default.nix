let sources = import ../../nix/sources.nix; in
{ pkgs
, nix-utils ? pkgs.callPackage sources.nix-utils {}
}:
let
  inherit (nix-utils) esc nameOfModuleWrapDir;
  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.hs;
  srcFile = pkgs.writeText "${name}.hs" src;

  ghc = pkgs.haskellPackages.ghcWithPackages (p: [
    p.directory
    p.filepath
    p.typed-process
    p.unix
  ]);
in
pkgs.runCommand name {} ''
  set -Eeuo pipefail
  ${esc ghc}/bin/ghc -Wall -O2 -o "$out" ${esc srcFile}
  ${esc pkgs.coreutils}/bin/chmod +x -- "$out"
''
