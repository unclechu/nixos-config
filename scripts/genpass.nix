args@{ ... }:
let pkgs-k = "pkgs"; utils-k = "utils"; config-k = "config"; in
assert let k = pkgs-k;  in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
assert let k = utils-k; in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
let
  pkgs = args."${pkgs-k}" or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { "${k}" = args."${k}".nixpkgs."${k}"; } else {}
  ));

  utils = args."${utils-k}" or (import ../../nix-utils-pick.nix args).pkg;
  inherit (utils) writeCheckedExecutable nameOfModuleFile;

  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  raku = "${pkgs.rakudo}/bin/raku";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable raku}
  '';

  pkg = writeCheckedExecutable name checkPhase ''
    #! ${raku}
    use v6.d;
    close $*IN;

    sub MAIN(Int $count = 8, Bool :$special-chars = False) {
      my Str @more-chars;

      @more-chars.append: '#@&$%_-=+~*^\|/;:,.?!'.split: q<>, :skip-empty
        if $special-chars;

      my \chars = (('a'..'z'), ('A'..'Z'), ('0'..'9'), @more-chars)
        .map({.list}).flat.grep({$_ ne '0' | 'O' | 'o'}).cache;

      (chars[chars.elems.rand.floor] for 1..$count).join.say;
    }
  '';
in
{
  inherit name pkg checkPhase;
}
