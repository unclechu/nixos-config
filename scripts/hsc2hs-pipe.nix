args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, gcc ? null
, ghc ? null
, ...
}:
let
  utils = import ../utils args;
  inherit (utils) esc writeCheckedExecutable wrapExecutable nameOfModuleFile;

  src = builtins.readFile "${(import ../apps/wenzels-bash.nix args).src}/apps/${name}";
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  ghc' = args.ghc or pkgs.haskellPackages.ghc;
  gcc' = args.gcc or pkgs.gcc;

  perl = "${pkgs.perl}/bin/perl";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable perl}
    ${utils.shellCheckers.fileIsExecutable "${ghc'}/bin/hsc2hs"}
    ${utils.shellCheckers.fileIsExecutable "${gcc'}/bin/gcc"}
  '';

  perlScript = writeCheckedExecutable name checkPhase "#! ${perl}\n${src}";

  perlDependencies = [
    pkgs.perlPackages.IPCSystemSimple
    pkgs.perlPackages.FileTemp
  ];

  pkg = wrapExecutable "${perlScript}/bin/${name}" {
    inherit checkPhase;
    deps = [ ghc' gcc' ];
    env = { PERL5LIB = pkgs.perlPackages.makePerlPath perlDependencies; };
  };
in
{
  inherit name src pkg checkPhase perlDependencies perlScript;
}
