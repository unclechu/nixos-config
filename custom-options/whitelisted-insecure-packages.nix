# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
args@{ config, pkgs, lib, ... }:

{
  options.whitelistedInsecurePackages =
    lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
      description =
        "A composable variant of “nixpkgs.config.permittedInsecurePackages”"
        + " (merges/concatenates between different modules)";
    };
}
