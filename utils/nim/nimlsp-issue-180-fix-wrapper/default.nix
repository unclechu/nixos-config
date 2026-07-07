# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{ pkgs ? import <nixpkgs> {}
, lib ? pkgs.lib
, stdenv ? pkgs.stdenv
, nim ? pkgs.nim
, nimlsp ? pkgs.nimlsp
}:

stdenv.mkDerivation (lib.fix (self: {
  pname = "nimlsp-issue-180-fix-wrapper";
  name = self.pname;
  meta.mainProgram = "nimlsp";
  src = ./nimlsp_issue_180_fix_wrapper.nim;
  dontUnpack = true;
  doCheck = true;
  nativeBuildInputs = [ nim ];
  buildPhase = ''
    runHook preBuild
    cp -- "$src" ${lib.escapeShellArg (baseNameOf self.src)}
    src=${lib.escapeShellArg (baseNameOf self.src)}
    nim c -d:release --nimcache:nimcache \
      -d:nimlspExecutable=${lib.escapeShellArg (lib.getExe nimlsp)} \
      -o:${lib.escapeShellArg self.meta.mainProgram} "$src"
    runHook postBuild
  '';
  installPhase = ''
    runHook preInstall
    mkdir -p -- "$out"/bin
    cp -- ${lib.escapeShellArg self.meta.mainProgram} "$out"/bin
    runHook postInstall
  '';
}))
