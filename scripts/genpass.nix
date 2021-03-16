let sources = import ../nix/sources.nix; in
{ pkgs
, nix-utils ? pkgs.callPackage sources.nix-utils {}
}:
let
  inherit (nix-utils) writeCheckedExecutable nameOfModuleFile;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  raku = "${pkgs.rakudo}/bin/raku";

  checkPhase = ''
    ${nix-utils.shellCheckers.fileIsExecutable raku}
  '';
in
writeCheckedExecutable name checkPhase ''
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
''
