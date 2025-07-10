# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
args@{ config, pkgs, lib, ... }:

let
  predicateFunctionType = lib.mkOptionType {
    name = "Package predicate function";
    description = "A predicate function that takes a package argument and returns a boolean";
    check = f: builtins.isFunction f && builtins.isBool (f pkgs.bash);
  };
in

{
  options.unfreePredicates =
    lib.mkOption {
      type = lib.types.listOf predicateFunctionType;
      default = [];
      description =
        "A composable list of “nixpkgs.config.allowUnfreePredicate” predicate functions"
        + " (if any of the predicates is “true” it means the package passess)";
    };
}
