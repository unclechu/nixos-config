args@{ ... }:
let pkgs-k = "pkgs"; utils-k = "utils"; config-k = "config"; in
assert let k = pkgs-k;  in builtins.hasAttr k args -> builtins.isAttrs args.${k};
assert let k = utils-k; in builtins.hasAttr k args -> builtins.isAttrs args.${k};
let
  pkgs = args.${pkgs-k} or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { ${k} = args.${k}.nixpkgs.${k}; } else {}
  ));

  utils = args.${utils-k} or (import ../nix-utils-pick.nix (
    pkgs.lib.filterAttrs (k: _: k == config-k) args // { inherit pkgs; }
  )).pkg;

  inherit (utils) esc writeCheckedExecutable wrapExecutable nameOfModuleFile;

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

  pkg = wrapExecutable "${perlScript}/bin/${name}" {
    env = { PERL5LIB = pkgs.perlPackages.makePerlPath perlDependencies; };
    inherit checkPhase;
  };
in
assert utils.valueCheckers.isNonEmptyString srcFile;
{
  inherit name src srcFile perlDependencies perlScript pkg;
}
