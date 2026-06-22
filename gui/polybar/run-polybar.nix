# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# Polybar runner script wrapped into a Nix derivation.
#
# It helps to not care about the script location.
# Just rely on the presence of “run-polybar” in the “PATH”.

{ lib
, callPackage
, stdenvNoCC
, makeBinaryWrapper
, shellcheck
, writeTextFile
, writeText
, bash
, polybar # Intended to be provided by “./polybar.nix”
, gnugrep
, coreutils
, inotify-tools
, diffutils

, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}
}:

let
  e = executable-dependencies {
    bash = bash;
    polybar = polybar;
    polybar-msg = polybar;
    inotifywait = inotify-tools;
    grep = gnugrep;
    cut = coreutils;
    diff = diffutils;
  };

  polybar-config-file =
    writeText "polybar-config-file" (builtins.readFile ./config.ini);

  run-polybar = stdenvNoCC.mkDerivation rec {
    name = "run-polybar";
    src = ./run-polybar.sh;

    nativeBuildInputs = [
      makeBinaryWrapper
      shellcheck
    ];

    dontUnpack = true;
    doCheck = true;

    checkPhase = ''
      runHook preCheck
      shellcheck -- "$src"
      ${e.checkPhase}
      runHook postCheck
    '';

    installPhase = ''
      runHook preInstall

      BIN_PATH="$out/bin/"${lib.escapeShellArg name}
      install -Dm755 -- "$src" "$BIN_PATH"

      CMD=(
        wrapProgram "$BIN_PATH"
        --prefix PATH : ${e.scriptDependenciesBinPath src}
        --set POLYBAR_CONFIG_FILE ${lib.escapeShellArg polybar-config-file}
      )
      "''${CMD[@]}"

      runHook postInstall
    '';
  };
in

run-polybar // { inherit polybar-config-file; }
