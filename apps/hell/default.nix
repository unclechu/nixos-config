# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# Shell-like Haskell REPL (GHCi).
# “Hell” is like “Shell” but without “S” and with “H” as a first letter that means “Haskell”.

{ pkgs ? import <nixpkgs> {}
, lib ? pkgs.lib
, writeText ? pkgs.writeText

, haskellPackages ? pkgs.haskellPackages

, __config ? builtins.readFile ./config.hs

, mk-generic-script ? pkgs.callPackage ../../utils/mk-generic-script.nix {}
, executable-dependencies ? pkgs.callPackage ../../utils/executable-dependencies.nix {}

, inNixShell ? false
}:

let
  esc = lib.escapeShellArg;

  ghc = haskellPackages.ghcWithPackages (p: [
    p.turtle

    p.text
    p.bytestring

    p.directory
    p.filepath
    p.unix
    p.filepath

    p.typed-process
    p.process

    p.async

    p.containers
    p.mtl
    p.foldl
    p.managed

    p.time
    p.aeson
    p.aeson-pretty
  ]);

  colorizedGhciScript = color:
    assert builtins.elem color ["red" "green"];
    let
      colorize = c: s:
        assert builtins.isInt c;
        assert builtins.isString s;
        ''\ESC[${toString c}m\STX${s}\ESC[m\STX'';
    in
    writeText "colorized-hell-ghci-script-${color}" ''
      ${__config}
      :set prompt "λ "
      :set prompt "${colorize (if color == "red" then 31 else 32) "λ"} "
    '';

  configNormalPrompt = colorizedGhciScript "green";
  configRootPrompt = colorizedGhciScript "red";

  e = executable-dependencies {
    dash = pkgs.dash;
    ghc = ghc;
    ghci = ghc;
    id = pkgs.coreutils;
  };

  ghciArgs = [
    "-ignore-dot-ghci"
    "-ghci-script"
    # Path to the configuration file here
  ];

  name = "hell";

  src = writeText "${name}-source" ''
    #! /usr/bin/env dash
    set -o errexit || exit; set -o nounset; set -o pipefail

    UID=$(${e.s.id} -u)
    if [ "$UID" = 0 ]
    then file=${esc configRootPrompt}
    else file=${esc configNormalPrompt}
    fi

    exec ${e.s.ghci} ${lib.escapeShellArgs ghciArgs} "$file" "$@"
  '';

  hell = mk-generic-script {
    inherit name src e;
    buildInputs = [ e.executables.dash ];
    dontAddDependencies = true;
    cutOffRuntimeDependenciesCheckPhase = null;

    checkPhase = ''(
      set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail

      # Required for the unicode to work properly,
      # there is plenty of unicode in the config file.
      export LC_ALL=C.UTF-8
      export LANG=C.UTF-8

      TEST_HELL_CMD='æøinproc ["echo", "hel" ‰ "lo", "world"] & stdout'

      OUTPUT=$(
        ${e.s.ghci} ${lib.escapeShellArgs ghciArgs} ${esc configNormalPrompt} 2>&1 \
          <<< "$TEST_HELL_CMD"
      )

      if >/dev/null grep -i error <<< "$OUTPUT"; then
        >&2 printf 'There are errors in the %s configuration:\n%s\n' \
          ${esc configNormalPrompt} "$OUTPUT"
        exit 1
      fi

      # Test that the expected substring is printed in the output
      if ! >/dev/null grep 'hello world' <<< "$OUTPUT"; then
        >&2 printf 'Could not find expected output substring after calling a test command (%s configuration):\n%s\n' \
          ${esc configNormalPrompt} "$OUTPUT"
        exit 1
      fi
    )'';
  };

  shell = pkgs.mkShell { buildInputs = builtins.attrValues e.executables; };
in

(if inNixShell then shell else hell) // { inherit hell shell; }
