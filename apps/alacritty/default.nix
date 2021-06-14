# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../../nix/sources.nix; in
{ callPackage
, writeText
, dash
, alacritty

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}

# Build options
, __configFile            ? ./alacritty.yml
, __darkColorsConfigFile  ? ./colors-dark.yml
, __lightColorsConfigFile ? ./colors-light.yml
}:
let
  inherit (__nix-utils) esc writeCheckedExecutable nameOfModuleWrapDir shellCheckers;
  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  dash-exe = "${dash}/bin/dash";
  alacritty-exe = "${alacritty}/bin/alacritty";

  checkPhase = ''
    ${shellCheckers.fileIsExecutable dash-exe}
    ${shellCheckers.fileIsExecutable alacritty-exe}
    ${shellCheckers.fileIsReadable "${__configFile}"}
    ${shellCheckers.fileIsReadable "${__darkColorsConfigFile}"}
    ${shellCheckers.fileIsReadable "${__lightColorsConfigFile}"}
  '';

  buildExecutable = name: colorsConfigFile:
    let
      builtConfigFile = writeText "${name}-config.yml" ''
        ${builtins.readFile "${__configFile}"}
        ${builtins.readFile "${colorsConfigFile}"}
      '';
    in
    writeCheckedExecutable name checkPhase ''
      #! ${dash-exe}
      ${esc alacritty-exe} --config-file=${esc builtConfigFile} "$@"
    '';

  default = buildExecutable name __darkColorsConfigFile;
in
default // {
  inherit default;
  dark  = buildExecutable "${name}-dark"  __darkColorsConfigFile;
  light = buildExecutable "${name}-light" __lightColorsConfigFile;
}
