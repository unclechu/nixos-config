# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ lib
, callPackage
, writeText

, dash
, nix
, ripgrep

, executable-dependencies ? callPackage ../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../utils/mk-generic-script.nix {}
}:

let
  name = "print-extra-gc-roots";

  e = executable-dependencies {
    dash = dash;
    nix-store = nix;
    rg = ripgrep;
  };

  excludeSystemRootsRegEx = (x: ''^"(${x})" -> \S+'') (builtins.concatStringsSep "|" [
    ''\{censored\}''
    ''/nix/var/nix/profiles/[^"]+''
    ''/run/(booted|current)-system''
    ''/home/[^/]+/(.cache/nix/flake-registry.json|.local/state/[^"]*home-manager[^"]+)''
  ]);

  src = writeText "${name}-source" ''
    #! /usr/bin/env dash
    set -o errexit || exit; set -o pipefail; set -o nounset; set -o xtrace
    ${e.s.nix-store} --gc --print-roots 2>/dev/null \
      | ${e.s.rg} -v -- ${lib.escapeShellArg excludeSystemRootsRegEx}
  '';
in

mk-generic-script {
  inherit name e src;
  buildInputs = [ e.executables.dash ];
  dontAddDependencies = true;
  cutOffRuntimeDependenciesCheckPhase = null;
}
