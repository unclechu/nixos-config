# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../../nix/sources.nix; in
{ lib
, callPackage
, stdenvNoCC
, makeBinaryWrapper
, shellcheck
, bash
, inotify-tools
, gnused
, dzen2
, coreutils

# Overridable dependencies
, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}

# Build options
, __srcScript ? ./main.bash
}:

let
  e = executable-dependencies {
    bash = bash;
    inotifywait = inotify-tools;
    sed = gnused;
    dzen2 = dzen2;
    rm = coreutils;
    stat = coreutils;
    sleep = coreutils;
    head = coreutils;
    touch = coreutils;
  };
in

stdenvNoCC.mkDerivation rec {
  name = "dzen-box";
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

  postPatch = ''
    _src=$(basename -- "$src")
    cp -- "$src" "$_src"
    # In runtime the script must spawn as fast as possible.
    # So removing the checks in runtime.
    sed -i '/# Guard dependencies/,/^$/d' "$_src"
  '';

  installPhase = ''
    runHook preInstall
    BIN_PATH="$out/bin/"${lib.escapeShellArg name}
    install -Dm755 -- "$_src" "$BIN_PATH"
    wrapProgram "$BIN_PATH" --prefix PATH : ${e.scriptDependenciesBinPath src}
    runHook postInstall
  '';
}
