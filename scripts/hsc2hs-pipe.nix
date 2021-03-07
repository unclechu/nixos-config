{ pkgs         ? import <nixpkgs> {}
, wenzels-bash ? import ../apps/wenzels-bash.nix { inherit pkgs config; }
, ghc          ? pkgs.haskellPackages.ghc
, gcc          ? pkgs.gcc

, config # Needed for “wenzels-bash” (set to “null” to use in Nix REPL)
}:
assert
  builtins.isPath       wenzels-bash.bashRC ||
  pkgs.lib.isDerivation wenzels-bash.bashRC ||
  pkgs.lib.isStorePath  wenzels-bash.bashRC;
assert pkgs.lib.isDerivation ghc;
assert pkgs.lib.isDerivation gcc;
import "${wenzels-bash.bashRC}/nix/scripts/hsc2hs-pipe.nix" { inherit pkgs ghc gcc; }
