{ pkgs  ? import <nixpkgs> {}
, utils ? import (import ../../nix/sources.nix).nix-utils { inherit pkgs; }
}:
let
  inherit (utils) esc writeCheckedExecutable nameOfModuleWrapDir;
  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.raku;
  raku = "${pkgs.rakudo}/bin/raku";
  xinput = "${pkgs.xlibs.xinput}/bin/xinput";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable raku}
    ${utils.shellCheckers.fileIsExecutable xinput}
  '';
in
writeCheckedExecutable name checkPhase ''
  #! ${raku}
  use v6.d;
  close $*IN;
  my \xinput := q<${xinput}>;
  ${src}
''
