# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ callPackage
, lib

, systemConfig # Needed for “wenzels-bash” (set to “null” to use in Nix REPL)

# Overridable dependencies
, __wenzels-bash ? callPackage ../apps/wenzels-bash.nix { inherit systemConfig; }
}:
assert
  builtins.isPath  __wenzels-bash.bashRC ||
  lib.isDerivation __wenzels-bash.bashRC ||
  lib.isStorePath  __wenzels-bash.bashRC;
callPackage "${__wenzels-bash.bashRC}/nix/scripts/timer.nix" {}
