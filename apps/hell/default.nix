# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# Shell-like Haskell REPL (GHCi).
# “Hell” is like “Shell” but without “S” and with “H” as a first letter that means “Haskell”.

{ pkgs ? import <nixpkgs> {}
, lib ? pkgs.lib

, haskellPackages ? pkgs.haskellPackages

, writeText ? pkgs.writeText
, writeShellApplication ? pkgs.writeShellApplication

, __config ? builtins.readFile ./config.hs

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
    p.typed-process
    p.process

    p.containers
    p.mtl
    p.foldl

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

  ghciCmd = [
    "ghci"
    "-ignore-dot-ghci"
    "-ghci-script"
    # Path to the configuration file here
  ];

  configNormalPrompt = colorizedGhciScript "green";
  configRootPrompt = colorizedGhciScript "red";

  dependencies = [ghc];

  hell = writeShellApplication {
    name = "hell";
    runtimeInputs = dependencies;

    text = ''
      if (( UID == 0 ))
      then file=${esc configRootPrompt}
      else file=${esc configNormalPrompt}
      fi

      exec ${lib.escapeShellArgs ghciCmd} "$file" "$@"
    '';

    checkPhase = ''(
      set -o nounset; set -o pipefail
      export PATH=${esc (lib.makeBinPath dependencies)}:"$PATH"

      # Required for the unicode to work properly,
      # there is plenty of unicode in the config file.
      export LC_ALL=C.UTF-8
      export LANG=C.UTF-8

      TEST_HELL_CMD='æøinproc ["echo", "hel" ‰ "lo", "world"] & stdout'

      OUTPUT=$(
        ${lib.escapeShellArgs ghciCmd} ${esc configNormalPrompt} 2>&1 \
          <<< "$TEST_HELL_CMD"
      )

      if >/dev/null grep -i error <<< "$OUTPUT"; then
        >&2 printf 'There are errors in the %s configuration:\n%s\n' ${esc configNormalPrompt} "$OUTPUT"
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

  shell = pkgs.mkShell { buildInputs = dependencies; };
in

(if inNixShell then shell else hell) // { inherit hell shell; }
