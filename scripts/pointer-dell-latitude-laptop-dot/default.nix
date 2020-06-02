args@{ ... }:
assert let k = "pkgs";  in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
assert let k = "utils"; in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
let
  pkgs = args.pkgs or (import <nixpkgs> {
    config = let k = "config"; in
      if builtins.hasAttr k args then {} else args."${k}".nixpkgs.config;
  });

  utils = args.utils or (import ../../nix-utils-pick.nix args).pkg;
  inherit (utils) esc writeCheckedExecutable nameOfModuleWrapDir;

  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.raku;

  raku = "${pkgs.rakudo}/bin/raku";
  xinput = "${pkgs.xlibs.xinput}/bin/xinput";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable raku}
    ${utils.shellCheckers.fileIsExecutable xinput}
  '';

  pkg = writeCheckedExecutable name checkPhase ''
    #! ${raku}
    use v6.d;
    close $*IN;
    my \xinput := q<${xinput}>;
    ${src}
  '';
in
{
  inherit name pkg checkPhase;
}
