{ pkgs  ? import <nixpkgs> {}
, utils ? import (import ../nix/sources.nix).nix-utils { inherit pkgs; }
}:
let
  inherit (utils) writeCheckedExecutable nameOfModuleFile;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  raku = "${pkgs.rakudo}/bin/raku";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable raku}
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
