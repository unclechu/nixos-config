# TODO Move this Nix config to “gpaste-gui” repo
let sources = import ../nix/sources.nix; in
{ pkgs  ? import <nixpkgs> {}
, utils ? import sources.nix-utils { inherit pkgs; }
}:
let
  inherit (utils) esc writeCheckedExecutable wrapExecutable nameOfModuleFile;
  srcFile = builtins.readFile "${sources.gpaste-gui}/${name}.pl";
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
