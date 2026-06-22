# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../../nix/sources.nix; in
{ lib
, callPackage
, stdenvNoCC
, makeBinaryWrapper
, shellcheck
, bash

# Overridable dependencies
, __xlib-keys-hack ? callPackage sources.xlib-keys-hack {}
, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}

# Build options
, __srcScript ? ./main.bash
}:
assert lib.isDerivation __xlib-keys-hack;
let
  esc = lib.escapeShellArg;
  src = builtins.readFile __srcScript;

  e = executable-dependencies {
    bash = bash;
    xlib-keys-hack = __xlib-keys-hack;
  };

  checkPhase = ''
    ${e.checkPhase}
  '';
in

stdenvNoCC.mkDerivation rec {
  name = "wenzels-xlib-keys-hack";
  src = __srcScript;

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
      --prefix PATH : ${e.scriptDependenciesBinPathWithIgnore src [
        # It must have root sticky bit, thus coming from system dependencies.
        "grant-access-to-input-devices"
      ]}
    )
    "''${CMD[@]}"

    runHook postInstall
  '';
}
