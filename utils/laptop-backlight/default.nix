{ pkgs  ? import <nixpkgs> {}
, utils ? import (import ../../nix/sources.nix).nix-utils { inherit pkgs; }
}:
let
  inherit (utils) esc nameOfModuleWrapDir;
  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.hs;
  srcFile = pkgs.writeText "${name}.hs" src;

  ghc = pkgs.haskellPackages.ghcWithPackages (p: [
    p.attoparsec
    p.filepath
  ]);
in
pkgs.runCommand name {} ''
  set -Eeuo pipefail
  ${esc ghc}/bin/ghc -Wall -O2 -o "$out" ${esc srcFile}
  ${esc pkgs.coreutils}/bin/chmod +x -- "$out"
''
