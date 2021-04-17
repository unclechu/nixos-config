# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../../nix/sources.nix; in
{ callPackage
, rakudo
, xlibs # Just for ‘xinput’

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}

# Build options
, __srcScript ? ./main.raku
}:
let
  inherit (__nix-utils) esc writeCheckedExecutable nameOfModuleWrapDir shellCheckers;
  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile __srcScript;
  raku = "${rakudo}/bin/raku";
  xinput = "${xlibs.xinput}/bin/xinput";

  checkPhase = ''
    ${shellCheckers.fileIsExecutable raku}
    ${shellCheckers.fileIsExecutable xinput}
  '';
in
writeCheckedExecutable name checkPhase ''
  #! ${raku}
  use v6.d;
  close $*IN;
  my \xinput := q<${xinput}>;
  ${src}
''
