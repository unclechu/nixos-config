# TODO Move this Nix config to “gpaste-gui” repo
let sources = import ../nix/sources.nix; in
{ pkgs
, nix-utils ? pkgs.callPackage sources.nix-utils {}
}:
let
  inherit (nix-utils) esc writeCheckedExecutable wrapExecutable nameOfModuleFile;
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
    ${nix-utils.shellCheckers.fileIsExecutable perl}
    ${nix-utils.shellCheckers.fileIsExecutable gpaste-client}
  '';

  perlScript = writeCheckedExecutable name checkPhase ''
    #! ${perl}
    use v5.24; use strict; use warnings;
    $ENV{PATH} = q<${pkgs.gnome3.gpaste}/bin:>.$ENV{PATH};
    ${srcFile}
  '';
in
assert nix-utils.valueCheckers.isNonEmptyString srcFile;
wrapExecutable "${perlScript}/bin/${name}" {
  env = { PERL5LIB = pkgs.perlPackages.makePerlPath perlDependencies; };
  inherit checkPhase;
}
