args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, ...
}:
let
  utils = import ../utils args;
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;

  src = fetchGit {
    url = "https://github.com/unclechu/gpaste-gui.git";
    rev = "b7a9d702a06ed7448e98661b7fa162922c29e17f"; # 28 April 2019
    ref = "master";
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
    ${utils.bash.checkFileIsExecutable perl}
    ${utils.bash.checkFileIsExecutable gpaste-client}
    ${utils.bash.checkValueIsNonEmptyString srcFile}
  '';

  perlScript = writeCheckedExecutable name checkPhase ''
    #! ${perl}
    use v5.24; use strict; use warnings;
    $ENV{PATH} = q<${pkgs.gnome3.gpaste}/bin:>.$ENV{PATH};
    ${srcFile}
  '';

  perlDepsPath = pkgs.perlPackages.makePerlPath perlDependencies;

  pkg = writeCheckedExecutable name checkPhase ''
    #! ${dash}
    PERL5LIB=${esc perlDepsPath} ${esc perlScript}/bin/${esc name} "$@"
  '';
in
{
  inherit name src srcFile perlDependencies perlScript pkg;
}
