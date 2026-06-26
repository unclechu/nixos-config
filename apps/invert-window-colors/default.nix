# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../../nix/sources.nix; in

{ lib
, callPackage
, mkShell
, symlinkJoin
, makeBinaryWrapper
, stdenv
, nim
, dbus
, pcre
, xdotool
, xwininfo

# nix-shell arguments
, inNixShell ? false

# Overridable dependencies
, nim-dbus-src ? sources.nim-dbus

# Build options
, __src ? ./.
}:

let
  src =
    builtins.filterSource (path: type:
      type == "regular" &&
      builtins.elem (baseNameOf path) [
        "app.nim"
        "ipc.nim"
        "main.nim"
        "types.nim"
      ]
    ) __src;

  invert-window-colors = stdenv.mkDerivation rec {
    pname = "invert-window-colors";
    name = pname;
    meta.mainProgram = name;
    inherit src;

    nativeBuildInputs = [
      nim
    ];

    buildInputs = [
      dbus
      pcre
    ];

    buildPhase = ''
      runHook preBuild
      (
        set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail

        BUILD_CMD=(
          nim c
          --nimcache:nimcache
          -p:${lib.escapeShellArg "${nim-dbus-src}"}
          -o:${lib.escapeShellArg meta.mainProgram}
          -d:nimOldCaseObjects
          main.nim
        )

        "''${BUILD_CMD[@]}"
      )
      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      (
        set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
        mkdir -p -- "$out"/bin
        cp -- ${lib.escapeShellArg meta.mainProgram} "$out"/bin
      )
      runHook postInstall
    '';
  };

  runtimeExecutableDependenies = [
    xdotool
    xwininfo
  ];

  invert-window-colors-wrapped = symlinkJoin rec {
    pname = "${lib.getName invert-window-colors}-wrapped";
    name = pname;
    meta.mainProgram = invert-window-colors.meta.mainProgram;
    nativeBuildInputs = [ makeBinaryWrapper ];
    paths = [ invert-window-colors ];
    postBuild = ''
      CMD=(
        wrapProgram
        "$out"/bin/${lib.escapeShellArg invert-window-colors.meta.mainProgram}
        --prefix PATH : ${lib.escapeShellArg (lib.makeBinPath runtimeExecutableDependenies)}
      )
      "''${CMD[@]}"
    '';
  };

  shell = mkShell {
    buildInputs =
      invert-window-colors.nativeBuildInputs
      ++ invert-window-colors.buildInputs
      ++ runtimeExecutableDependenies
      ;
  };
in

(if inNixShell then shell else invert-window-colors-wrapped) // {
  invert-window-colors = invert-window-colors-wrapped;
  invert-window-colors-unwrapped = invert-window-colors;
  inherit shell;
}
