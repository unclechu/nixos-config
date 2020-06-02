args@{ ... }:
assert let k = "pkgs";  in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
assert let k = "utils"; in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
let
  pkgs = args.pkgs or (import <nixpkgs> {
    config = let k = "config"; in
      if builtins.hasAttr k args then {} else args."${k}".nixpkgs.config;
  });

  ghc-name = "ghc";
  gcc-name = "gcc";
  optionalAppArgs = [ ghc-name gcc-name ];

  optionalAppArgsAssertion =
    let f = a: k: assert builtins.hasAttr k args -> pkgs.lib.isDerivation args."${k}"; a+1;
    in builtins.foldl' f 0 optionalAppArgs;
in
assert optionalAppArgsAssertion == builtins.length optionalAppArgs;
assert builtins.hasAttr ghc-name pkgs.haskellPackages;
assert builtins.hasAttr gcc-name pkgs;
let
  utils = args.utils or (import ../nix-utils-pick.nix args).pkg;
  inherit (utils) esc writeCheckedExecutable wrapExecutable nameOfModuleFile;

  src = builtins.readFile "${(import ../apps/wenzels-bash.nix args).src}/apps/${name}";
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  perl = "${pkgs.perl}/bin/perl";
  ghc = args."${ghc-name}" or pkgs.haskellPackages."${ghc-name}";
  gcc = args."${gcc-name}" or pkgs."${gcc-name}";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable perl}
    ${utils.shellCheckers.fileIsExecutable "${ghc}/bin/hsc2hs"}
    ${utils.shellCheckers.fileIsExecutable "${gcc}/bin/gcc"}
  '';

  perlScript = writeCheckedExecutable name checkPhase "#! ${perl}\n${src}";

  perlDependencies = [
    pkgs.perlPackages.IPCSystemSimple
    pkgs.perlPackages.FileTemp
  ];

  pkg = wrapExecutable "${perlScript}/bin/${name}" {
    inherit checkPhase;
    deps = [ ghc gcc ];
    env = { PERL5LIB = pkgs.perlPackages.makePerlPath perlDependencies; };
  };
in
{
  inherit name src pkg checkPhase perlDependencies perlScript;
}
