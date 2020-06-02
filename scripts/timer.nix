args@{ ... }:
assert let k = "pkgs";  in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
assert let k = "utils"; in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
let
  pkgs = args.pkgs or (import <nixpkgs> {
    config = let k = "config"; in
      if builtins.hasAttr k args then {} else args."${k}".nixpkgs.config;
  });

  utils = args.utils or (import ../nix-utils-pick.nix args).pkg;
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;

  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile "${(import ../apps/wenzels-bash.nix args).src}/apps/${name}.pl6";

  raku = "${pkgs.rakudo}/bin/raku";
  dzen2 = "${pkgs.dzen2}/bin/dzen2";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable raku}
    ${utils.shellCheckers.fileIsExecutable dzen2}
  '';

  pkg = writeCheckedExecutable name checkPhase ''
    #! ${raku}
    use v6.d;
    %*ENV{'PATH'} = q<${pkgs.dzen2}/bin> ~ ':' ~ %*ENV{'PATH'};
    ${builtins.replaceStrings ["use v6;"] [""] src}
  '';
in
{
  inherit src name pkg checkPhase;
}
