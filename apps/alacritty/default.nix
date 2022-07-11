# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../../nix/sources.nix; in
{ callPackage
, writeTextFile
, lib

, bash
, alacritty
, yaml2json
, json2yaml
, jq

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}

# Build options
, __configSrc            ? ./alacritty.yml    # A derivation or a path
, __darkColorsConfigSrc  ? ./colors-dark.yml  # A derivation or a path
, __lightColorsConfigSrc ? ./colors-light.yml # A derivation or a path
}:
let
  inherit (__nix-utils) shellCheckers;
  esc = lib.escapeShellArg;
  name = "alacritty";

  e = {
    bash = "${bash}/bin/bash";
    alacritty = "${alacritty}/bin/alacritty";
    yaml2json = "${yaml2json}/bin/yaml2json";
    json2yaml = "${json2yaml}/bin/json2yaml";
    jq = "${jq}/bin/jq";
  };

  es = builtins.mapAttrs (lib.const lib.escapeShellArg) e;

  overridableConfig = ''
    ${es.yaml2json} \
      | ${es.jq} -s --argjson x "$local_cfg" --argjson y "$local_color_cfg" '.[0] * $x * $y' \
      | ${es.json2yaml}
  '';

  basicCheckPhase = ''
    ${builtins.concatStringsSep "\n" (map shellCheckers.fileIsExecutable (builtins.attrValues e))}
    ${shellCheckers.fileIsReadable "${__configSrc}"}
    ${shellCheckers.fileIsReadable "${__darkColorsConfigSrc}"}
    ${shellCheckers.fileIsReadable "${__lightColorsConfigSrc}"}
  '';

  buildExecutable = name: color: config: writeTextFile {
    name = assert builtins.isString name; name;
    executable = true;
    destination = "/bin/${name}";

    text = assert builtins.elem color [ "dark" "light" ]; ''
      #! ${e.bash}
      set -o errexit || exit
      set -o nounset
      set -o pipefail

      LOCAL_CFG_FILE=$HOME/.wenzels-alacritty-local-config.yml
      LOCAL_COLOR_CFG_FILE=$HOME/.wenzels-alacritty-${esc color}-local-config.yml

      local_cfg='{}' # JSON
      local_color_cfg='{}' # JSON
      if [[ -f $LOCAL_CFG_FILE ]]; then
        local_cfg=$(${es.yaml2json} < "$LOCAL_CFG_FILE")
      fi
      if [[ -f $LOCAL_COLOR_CFG_FILE ]]; then
        local_color_cfg=$(${es.yaml2json} < "$LOCAL_COLOR_CFG_FILE")
      fi

      exec ${es.alacritty} --config-file=<((${overridableConfig}) < ${esc config}) "$@"
    '';

    checkPhase = ''
      ${basicCheckPhase}
      ${shellCheckers.fileIsReadable config}
      (
        set -o nounset
        set -o pipefail
        local_cfg='{"foo":{"abc":10}}'
        local_color_cfg='{"foo":{"def":20}}'
        x=$((${overridableConfig}) <<< $'foo:\n bar:\n  baz: 123' | ${es.yaml2json} | ${es.jq} -cS)
        f () { [[ $x == '{"foo":{"abc":10,"bar":{"baz":123},"def":20}}' ]]; }
        f || (set -o xtrace; f)
      )
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
        buildExecutable name "dark"
          (buildConfig darkName __darkColorsConfigSrc defaultConfig font);
    in
    default // {
      inherit default;
      dark =
        buildExecutable darkName "dark"
          (buildConfig darkName __darkColorsConfigSrc darkConfig font);
      light =
        buildExecutable lightName "light"
          (buildConfig lightName __lightColorsConfigSrc lightConfig font);
    };
in
customize {} // {
  inherit customize;
  mainConfigSrc        = __configSrc;
  darkColorsConfigSrc  = __darkColorsConfigSrc;
  lightColorsConfigSrc = __lightColorsConfigSrc;
}
