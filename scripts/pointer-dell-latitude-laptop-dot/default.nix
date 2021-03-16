let sources = import ../../nix/sources.nix; in
{ pkgs
, nix-utils ? pkgs.callPackage sources.nix-utils {}
}:
let
  inherit (nix-utils) esc writeCheckedExecutable nameOfModuleWrapDir;
  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.raku;
  raku = "${pkgs.rakudo}/bin/raku";
  xinput = "${pkgs.xlibs.xinput}/bin/xinput";

  checkPhase = ''
    ${nix-utils.shellCheckers.fileIsExecutable raku}
    ${nix-utils.shellCheckers.fileIsExecutable xinput}
  '';
in
writeCheckedExecutable name checkPhase ''
  #! ${raku}
  use v6.d;
  close $*IN;
  my \xinput := q<${xinput}>;
  ${src}
''
