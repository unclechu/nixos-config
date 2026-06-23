# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ callPackage
, writeText
, rakudo

, executable-dependencies ? callPackage ../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../utils/mk-generic-script.nix {}
}:

let
  name = "genpass";

  e = executable-dependencies {
    raku = rakudo;
  };

  src = writeText "${name}-source" ''
    #! /usr/bin/env raku
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

mk-generic-script {
  inherit name src e;
  buildInputs = [ e.executables.raku ];
  lintBuildInputs = [ e.executables.raku ];
  dontAddDependencies = true;
  cutOffRuntimeDependenciesCheckPhase = null;

  lintPhase = ''
    raku -c -- "$src"
  '';
}
