# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../../nix/sources.nix; in
{ lib
, callPackage
, runCommand
, writeText
, coreutils
, hlint
, haskellPackages

# Overridable dependencies
, executable-dependencies ? callPackage ../executable-dependencies.nix {}

# Build options
, __srcFile ? ./main.hs
}:
let
  name = "grant-access-to-input-devices";
  src = builtins.readFile __srcFile;
  srcFile = writeText "${name}.hs" src;

  ghc = haskellPackages.ghcWithPackages (p: [
    p.directory
    p.filepath
    p.typed-process
    p.unix
  ]);

  e = executable-dependencies {
    chmod = coreutils;
    ghc = ghc;
    hlint = hlint;
  };
in
runCommand name {} ''
  set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
  ${e.checkPhase}
  ${e.s.hlint} -- ${lib.escapeShellArg srcFile}
  ${e.s.ghc} -Wall -O2 -o "$out" ${lib.escapeShellArg srcFile}
  ${e.s.chmod} +x -- "$out"
''
