args@{ ... }:
let pkgs-k = "pkgs"; utils-k = "utils"; config-k = "config"; in
assert let k = pkgs-k;  in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
assert let k = utils-k; in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
let
  pkgs = args."${pkgs-k}" or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { "${k}" = args."${k}".nixpkgs."${k}"; } else {}
  ));

  ghc-k = "ghc";
  gcc-k = "gcc";
  optionalAppArgs = [ ghc-k gcc-k ];

  optionalAppArgsAssertion =
    let f = a: k: assert builtins.hasAttr k args -> pkgs.lib.isDerivation args."${k}"; a+1;
    in builtins.foldl' f 0 optionalAppArgs;
in
assert optionalAppArgsAssertion == builtins.length optionalAppArgs;
assert builtins.hasAttr ghc-k pkgs.haskellPackages;
assert builtins.hasAttr gcc-k pkgs;
let
  utils = args."${utils-k}" or (import ../../nix-utils-pick.nix args).pkg;
  inherit (utils) esc writeCheckedExecutable wrapExecutable nameOfModuleFile;

  src = builtins.readFile "${(import ../apps/wenzels-bash.nix args).src}/apps/${name}";
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  perl = "${pkgs.perl}/bin/perl";
  ghc = let k = ghc-k; in args."${k}" or pkgs.haskellPackages."${k}";
  gcc = let k = gcc-k; in args."${k}" or pkgs."${k}";

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
