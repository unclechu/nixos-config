let sources = import ../../nix/sources.nix; in
{ callPackage
, runCommand
, writeText
, coreutils
, haskellPackages

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}

# Build options
, __srcFile ? ./main.hs
}:
let
  inherit (__nix-utils) esc nameOfModuleWrapDir;
  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile __srcFile;
  srcFile = writeText "${name}.hs" src;

  ghc = haskellPackages.ghcWithPackages (p: [
    p.directory
    p.filepath
    p.typed-process
    p.unix
  ]);
in
runCommand name {} ''
  set -Eeuo pipefail || exit
  ${esc ghc}/bin/ghc -Wall -O2 -o "$out" ${esc srcFile}
  ${esc coreutils}/bin/chmod +x -- "$out"
''
