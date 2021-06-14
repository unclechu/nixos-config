# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../../nix/sources.nix; in
{ callPackage
, writeTextFile
, lib
, dash
, alacritty

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}

# Build options
, __configSrc            ? ./alacritty.yml    # A derivation or a path
, __darkColorsConfigSrc  ? ./colors-dark.yml  # A derivation or a path
, __lightColorsConfigSrc ? ./colors-light.yml # A derivation or a path
}:
let
  inherit (__nix-utils) esc wrapExecutable nameOfModuleWrapDir shellCheckers;
  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  dash-exe = "${dash}/bin/dash";
  alacritty-exe = "${alacritty}/bin/alacritty";

  basicCheckPhase = ''
    ${shellCheckers.fileIsExecutable dash-exe}
    ${shellCheckers.fileIsExecutable alacritty-exe}
    ${shellCheckers.fileIsReadable "${__configSrc}"}
    ${shellCheckers.fileIsReadable "${__darkColorsConfigSrc}"}
    ${shellCheckers.fileIsReadable "${__lightColorsConfigSrc}"}
  '';

  buildExecutable = name: config:
    assert builtins.isString name;
    wrapExecutable alacritty-exe {
      inherit name;
      args = ["--config-file=${config}"];

      checkPhase = ''
        ${basicCheckPhase}
        ${shellCheckers.fileIsReadable config}
      '';
    } // { inherit config; };

  buildConfig = name: colorsConfigFile: optionalFullConfig: font:
    assert builtins.isString name;
    assert ! isNull font -> builtins.isString font;
    if ! isNull optionalFullConfig && ! builtins.isString optionalFullConfig
    then
      optionalFullConfig
    else
      writeTextFile {
        name = "${name}-config.yml";

        text =
          let
            text =
              if ! isNull optionalFullConfig && ! builtins.isString optionalFullConfig
              then builtins.readFile "${optionalFullConfig}"
              else

              if ! isNull optionalFullConfig && builtins.isString optionalFullConfig
              then optionalFullConfig
              else ''
                ${builtins.readFile "${__configSrc}"}
                ${builtins.readFile "${colorsConfigFile}"}
              '';

            replaceFont =
              if isNull font
              then lib.id
              else builtins.replaceStrings ["Hack"] ["\"${font}\""];
          in
            replaceFont text;

        checkPhase = ''
          set -Eeuo pipefail || exit
          ${shellCheckers.fileIsReadable "${__configSrc}"}
          ${shellCheckers.fileIsReadable "${colorsConfigFile}"}
        '';
      };

  customize =
    { defaultName ? name
    , darkName    ? "${defaultName}-dark"
    , lightName   ? "${defaultName}-light"

    , font ? null # “Hack” by default

    , defaultConfig ? null # Optional derivation or a string
    , darkConfig    ? null # Optional derivation or a string
    , lightConfig   ? null # Optional derivation or a string
    }:
    assert builtins.isString defaultName;
    assert builtins.isString darkName;
    assert builtins.isString lightName;
    assert ! isNull font -> builtins.isString font;
    let
      default =
        buildExecutable name (buildConfig darkName __darkColorsConfigSrc defaultConfig font);
    in
    default // {
      inherit default;
      dark =
        buildExecutable darkName (buildConfig darkName __darkColorsConfigSrc darkConfig font);
      light =
        buildExecutable lightName (buildConfig lightName __lightColorsConfigSrc lightConfig font);
    };
in
customize {} // {
  inherit customize;
  mainConfigSrc        = __configSrc;
  darkColorsConfigSrc  = __darkColorsConfigSrc;
  lightColorsConfigSrc = __lightColorsConfigSrc;
}
