args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, ghc ? null
, ...
}:
let
  utils = import ../utils args;
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;

  src = builtins.readFile "${(import ../apps/wenzels-bash.nix args).src}/apps/${name}";
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  ghc' = args.ghc or pkgs.haskellPackages.ghc;

  dash = "${pkgs.dash}/bin/dash";
  perl = "${pkgs.perl}/bin/perl";
  hsc2hs = "${ghc'}/bin/hsc2hs";

  checkPhase = ''
    ${utils.bash.checkFileIsExecutable dash}
    ${utils.bash.checkFileIsExecutable perl}
    ${utils.bash.checkFileIsExecutable hsc2hs}
  '';

  perlScript = writeCheckedExecutable name checkPhase ''
    #! ${perl}
    use v5.10; use strict; use warnings; use autodie qw(:all);
    $ENV{PATH} = q<${ghc'}/bin:>.$ENV{PATH};
    ${src}
  '';

  perlDependencies = [
    pkgs.perlPackages.IPCSystemSimple
    pkgs.perlPackages.FileTemp
  ];

  perlDepsPath = pkgs.perlPackages.makePerlPath perlDependencies;

  pkg = writeCheckedExecutable name checkPhase ''
    #! ${dash}
    PERL5LIB=${esc perlDepsPath} ${esc perlScript}/bin/${esc name} "$@"
  '';
in
{
  inherit name src pkg checkPhase perlDependencies perlScript;
}
