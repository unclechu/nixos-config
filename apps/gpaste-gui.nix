# TODO Move this Nix config to “gpaste-gui” repo
{ pkgs  ? import <nixpkgs> {}
, utils ? import (import ../nix/sources.nix).nix-utils { inherit pkgs; }
}:
let
  inherit (utils) esc writeCheckedExecutable wrapExecutable nameOfModuleFile;

  # TODO Pin using “niv”
  src = pkgs.fetchFromGitHub {
    owner = "unclechu";
    repo = "gpaste-gui";
    rev = "b7a9d702a06ed7448e98661b7fa162922c29e17f"; # ref "master", 28 April 2019
    sha256 = "0926ffw7lhvd2ihzwjis9nd9by6dwxkz85vsl2d7vxdq349al8q7";
  };

  srcFile = builtins.readFile "${src}/${name}.pl";
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  dash = "${pkgs.dash}/bin/dash";
  perl = "${pkgs.perl}/bin/perl";
  gpaste-client = "${pkgs.gnome3.gpaste}/bin/gpaste-client";

  perlDependencies = [
    pkgs.perlPackages.GetoptLong
    pkgs.perlPackages.PodUsage
    pkgs.perlPackages.Glib
    pkgs.perlPackages.Gtk2

    # Sub dependencies
    pkgs.perlPackages.Pango
    pkgs.perlPackages.Cairo
  ];

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable perl}
    ${utils.shellCheckers.fileIsExecutable gpaste-client}
  '';

  perlScript = writeCheckedExecutable name checkPhase ''
    #! ${perl}
    use v5.24; use strict; use warnings;
    $ENV{PATH} = q<${pkgs.gnome3.gpaste}/bin:>.$ENV{PATH};
    ${srcFile}
  '';
in
assert utils.valueCheckers.isNonEmptyString srcFile;
wrapExecutable "${perlScript}/bin/${name}" {
  env = { PERL5LIB = pkgs.perlPackages.makePerlPath perlDependencies; };
  inherit checkPhase;
}
