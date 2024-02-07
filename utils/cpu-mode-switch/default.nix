# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../../nix/sources.nix; in
{ callPackage
, runCommand
, mkShell
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
    p.unix
  ]);

  shell = mkShell {
    buildInputs = [
      coreutils
      ghc
    ];
  };
in

runCommand name {} ''
  set -o nounset
  ${esc ghc}/bin/ghc -o "$out" ${esc srcFile}
  ${esc coreutils}/bin/chmod +x -- "$out"
''

//

{ inherit shell; }
