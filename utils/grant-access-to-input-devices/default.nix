args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, ...
}:
let
  utils = import ../. args;
  inherit (utils) esc nameOfModuleWrapDir;

  ghc = pkgs.haskellPackages.ghcWithPackages (p: [
    p.directory
    p.filepath
    p.typed-process
    p.unix
  ]);

  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.hs;
  srcFile = pkgs.writeText "${name}.hs" src;

  pkg = pkgs.runCommand name {} ''
    set -Eeuo pipefail
    ${esc ghc}/bin/ghc -Wall -O2 -o "$out" ${esc srcFile}
    ${esc pkgs.coreutils}/bin/chmod +x -- "$out"
  '';
in
{
  inherit name pkg ghc;
}
