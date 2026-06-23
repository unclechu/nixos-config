# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../../nix/sources.nix; in
{ lib
, callPackage
, runCommand
, mkShell
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
  name = "cpu-mode-switch";
  src = builtins.readFile __srcFile;
  srcFile = writeText "${name}.hs" src;

  ghc = haskellPackages.ghcWithPackages (p: [
    p.directory
    p.unix
  ]);

  e = executable-dependencies {
    chmod = coreutils;
    ghc = ghc;
    hlint = hlint;
  };

  shell = mkShell {
    buildInputs = lib.unique (builtins.attrValues e.executables);
  };
in

runCommand name {} ''
  set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
  ${e.checkPhase}
  ${e.s.hlint} -- ${lib.escapeShellArg srcFile}
  ${e.s.ghc} -Wall -O2 -o "$out" ${lib.escapeShellArg srcFile}
  ${e.s.chmod} +x -- "$out"
''

//

{ inherit shell; }
