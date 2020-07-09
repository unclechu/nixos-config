args@{ ... }:
let pkgs-k = "pkgs"; config-k = "config"; in
assert let k = pkgs-k; in builtins.hasAttr k args -> builtins.isAttrs args.${k};
let
  pkgs = args.${pkgs-k} or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { ${k} = args.${k}.nixpkgs.${k}; } else {}
  ));

  ghc-k = "ghc";
  gcc-k = "gcc";
  optionalAppArgs = [ ghc-k gcc-k ];

  optionalAppArgsAssertion =
    let f = a: k: assert builtins.hasAttr k args -> pkgs.lib.isDerivation args.${k}; a+1;
    in builtins.foldl' f 0 optionalAppArgs;
in
assert optionalAppArgsAssertion == builtins.length optionalAppArgs;
assert builtins.hasAttr ghc-k pkgs.haskellPackages;
assert builtins.hasAttr gcc-k pkgs;
let
  wenzels-bash = import ../apps/wenzels-bash.nix args;

  ghc = let k = ghc-k; in args.${k} or pkgs.haskellPackages.${k};
  gcc = let k = gcc-k; in args.${k} or pkgs.${k};
in
import "${wenzels-bash.bashRC}/nix/scripts/hsc2hs-pipe.nix" { inherit pkgs ghc gcc; }
