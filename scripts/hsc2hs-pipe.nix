{ callPackage
, lib

# Forwarded arguments
, systemConfig # Needed for “wenzels-bash” (set to “null” to use in Nix REPL)

# Overridable dependencies
, __wenzels-bash ? callPackage ../apps/wenzels-bash.nix { inherit systemConfig; }
}:
assert
  builtins.isPath  __wenzels-bash.bashRC ||
  lib.isDerivation __wenzels-bash.bashRC ||
  lib.isStorePath  __wenzels-bash.bashRC;
callPackage "${__wenzels-bash.bashRC}/nix/scripts/hsc2hs-pipe.nix" {}
