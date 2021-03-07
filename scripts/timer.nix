{ pkgs         ? import <nixpkgs> {}
, wenzels-bash ? import ../apps/wenzels-bash.nix { inherit pkgs config; }

, config # Needed for “wenzels-bash” (set to “null” to use in Nix REPL)
}:
assert
  builtins.isPath       wenzels-bash.bashRC ||
  pkgs.lib.isDerivation wenzels-bash.bashRC ||
  pkgs.lib.isStorePath  wenzels-bash.bashRC;
import "${wenzels-bash.bashRC}/nix/scripts/timer.nix" { inherit pkgs; }
