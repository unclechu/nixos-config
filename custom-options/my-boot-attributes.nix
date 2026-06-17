# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
args@{ lib, ... }:
{
  options.my-boot-attributes = {
    kernelModules = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
    };

    kernelParams = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [];
    };
  };
}
