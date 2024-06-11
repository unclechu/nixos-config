# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../../nix/sources.nix; in
{ callPackage
, writeTextFile
, runCommand
, lib

, bash
, alacritty
, jq # Merging multiple Alacritty config pieces and doing some replacements
, clunky-toml-json-converter ? callPackage ../clunky-toml-json-converter {}

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}

# Build options
# Main configuration of Alacritty
, __configSrc            ? ./alacritty.toml    # A derivation or a path
# Colors configuration for the “dark” color scheme
, __darkColorsConfigSrc  ? ./colors-dark.toml  # A derivation or a path
# Colors configuration for the “ligth” color scheme
, __lightColorsConfigSrc ? ./colors-light.toml # A derivation or a path
}:
let
  inherit (__nix-utils) shellCheckers;
  esc = lib.escapeShellArg;
  name = "alacritty";

  # Executable dependencies mapping (name → full executable path)
  e = {
    bash = "${bash}/bin/bash";
    alacritty = "${alacritty}/bin/alacritty";
    clunky-toml-json-converter =
      "${clunky-toml-json-converter}/bin/clunky-toml-json-converter";
    jq = "${jq}/bin/jq";
  };

  # Executables mapping with string shell-escaping for the executable paths
  es = builtins.mapAttrs (lib.const lib.escapeShellArg) e;

  basicCheckPhase = ''
    ${builtins.concatStringsSep "\n" (map shellCheckers.fileIsExecutable (builtins.attrValues e))}
    ${shellCheckers.fileIsReadable "${__configSrc}"}
    ${shellCheckers.fileIsReadable "${__darkColorsConfigSrc}"}
    ${shellCheckers.fileIsReadable "${__lightColorsConfigSrc}"}
  '';

  colorAssertion = color: builtins.elem color [ "dark" "light" ];

  # File name template for local configuration customization.
  #
  # The wrapper produced by “buildExecutable” looks for this file in your HOME directory.
  # If the file exists it will be appended to the main configuration.
  #
  # This local config can be useful for example to customize font size when switching to a different
  # display with different DPI, and re-adjusted font size does not work for you.
  #
  # Arguments:
  # - color (null or string) — “dark” or “light” (see the corresponding colors config TOML files).
  #   Set to null for “main” customization (will be applied for default configuration and all color
  #   configurations too, regardless of presence of per-color local customization configuration
  #   file). Think of “color=null” as of extension for “__configSrc” and when it is not null as of
  #   extension to either “__darkColorsConfigSrc” or “__lightColorsConfigSrc” based on the “color”.
  #
  # Returns a string with the file name (just file name).
  localConfigFileName = color:
    assert ! isNull color -> colorAssertion color;
    ".wenzels-alacritty${if isNull color then "" else "-${color}"}-local-config.toml";

  # Build Alacritty executable wrapper with attached config file via command-line arguments.
  #
  # Take configuration file and build Alacritty executable wrapper with that config file attached
  # with an ability to extend it with a local custom configuration during runtime (see
  # “localConfigFileName”).
  #
  # Arguments:
  # - name (string) — Name of the new executable/wrapper (e.g. “alacritty-dark-colors”)
  # - color (string) — “dark” or “light” (see the corresponding colors config TOML files).
  #   This value only matters here for the local customization config file lookup (see
  #   “localConfigFileName"). You call “buildExecutable” with already patched “config” argument for
  #   one color scheme or another, and only make here using this argument to lookup for local
  #   customization per-color config, so that you can have different local configuration
  #   customization for each color scheme.
  # - jsonConfig (derivation) — New JSON (not TOML) config file derivation
  #   (created by “buildConfig” function)
  #
  # Returns a derivation of the Alacritty executable wrapper with attached configuration file.
  buildExecutable = name: color: jsonConfig: writeTextFile {
    name = assert builtins.isString name; name;
    executable = true;
    destination = "/bin/${name}";

    text = assert colorAssertion color; let
      # Dynamic generation of the TOML configuration deep-merging it with the main configuration
      tomlConfigMergedWithLocalConfigs = ''
        ${es.jq} -n \
        --argjson mainConfig "$MAIN_CFG_JSON" \
        --argjson localMainExtraConfig "$LOCAL_CFG_JSON" \
        --argjson localColorsExtraConfig "$LOCAL_COLORS_CFG_JSON" \
        '$mainConfig * $localMainExtraConfig * $localColorsExtraConfig' \
        | ${es.clunky-toml-json-converter} json2toml
      '';
    in ''
      #! ${e.bash}
      set -o errexit || exit
      set -o nounset
      set -o pipefail

      MAIN_CFG_JSON=$(<${esc jsonConfig})

      LOCAL_CFG_TOML_FILE=$HOME/${esc (localConfigFileName null)}
      LOCAL_COLORS_CFG_TOML_FILE=$HOME/${esc (localConfigFileName color)}

      LOCAL_CFG_JSON='{}' # File contents (JSON object)
      LOCAL_COLORS_CFG_JSON='{}' # File contents (JSON object)
      if [[ -f $LOCAL_CFG_TOML_FILE ]]; then
        LOCAL_CFG_JSON=$(
          <"$LOCAL_CFG_TOML_FILE" ${es.clunky-toml-json-converter} toml2json
        )
      fi
      if [[ -f $LOCAL_COLORS_CFG_TOML_FILE ]]; then
        LOCAL_COLORS_CFG_JSON=$(
          <"$LOCAL_COLORS_CFG_TOML_FILE" ${es.clunky-toml-json-converter} toml2json
        )
      fi

      exec ${es.alacritty} --config-file=<(${tomlConfigMergedWithLocalConfigs}) "$@"
    '';

    checkPhase = ''
      ${basicCheckPhase}
      ${shellCheckers.fileIsReadable jsonConfig}
    '';
  } // { inherit jsonConfig; };

  # Build Alacritty configuration JSON file (not TOML) derivation.
  #
  # Arguments:
  # - name (string) — The base name of the executable (e.g. “alacritty-dark”, which will result into
  #   “alacritty-dark-config.toml” for the produced derivation).
  # - colorsConfigFile (readable file as a Nix path or derivation) — Colors configuration file
  #   (to extend “__configSrc” with)
  # - fontFamily (null or a string) — Font name to set in the config
  #   (e.g. "IosevkaTerm Nerd Font Mono"), set to null to keep the default font family
  #   (set in “__configSrc”)
  #
  # Returns Alacritty configuration file (derivation) in JSON format.
  # When running Alacritty configured wrapper the file is read and merged with optionally present
  # local customization configuration file. It has to be read as JSON so that it can be deep-merged
  # with the local customization file using “jq”. So there is no point to converting it to TOML
  # (which Alacritty takes) yet here, otherwise there would be a need for extra redundant conversion
  # each run.
  buildConfig = name: colorsConfigFile: fontFamily:
    assert builtins.isString name;
    assert ! isNull fontFamily -> builtins.isString fontFamily;
    let
      mainConfig = builtins.readFile "${__configSrc}";
      colorsConfig = builtins.readFile "${colorsConfigFile}";
    in
    runCommand "${name}-config.json" {
      # If set to null will result into variable being undefined
      customFontFamily = fontFamily;
    } ''
      set -o nounset
      set -o pipefail

      # Convert the configs first from TOML to JSON for “jq” use
      MAIN_CONFIG_JSON=$(${es.clunky-toml-json-converter} toml2json <<< ${esc mainConfig})
      COLORS_CONFIG_JSON=$(${es.clunky-toml-json-converter} toml2json <<< ${esc colorsConfig})

      # Merge main config with color scheme-related config
      MERGE_CONFIGS_CMD=(
        ${es.jq} -n
        --argjson mainConfig "$MAIN_CONFIG_JSON"
        --argjson colorsConfig "$COLORS_CONFIG_JSON"
        '$mainConfig * $colorsConfig'
      )
      MERGED_CONFIG_JSON=$("''${MERGE_CONFIGS_CMD[@]}")

      if [[ -v customFontFamily ]]; then
        PRODUCED_CONFIG=$(
          <<< "$MERGED_CONFIG_JSON" \
          ${es.jq} \
          --arg fontFamily "$customFontFamily" \
          '.font.normal.family = $fontFamily'
        )
      else
        PRODUCED_CONFIG=$MERGED_CONFIG_JSON
      fi

      printf '%s\n' "$PRODUCED_CONFIG" > "$out"
    '';

  # An interface to produce Alacritty wrappers with attached customized configuration files.
  #
  # For the input arguments see the single arguemnt attribute set destructurization below.
  #
  # Returns a derivation with “default” wrapper (“default” configuration is one or the other color
  # scheme picked as “default”). This derivation is extended with “dark” and “light” attributes that
  # are set to wrappers with attached corresponding color scheme configurations.
  customize =
    {
    # Name for the “default” color scheme executable
    defaultName ? name
    # Name for the “dark” color scheme executable
    , darkName ? "${defaultName}-dark"
    # Name for the “light” color scheme executable
    , lightName ? "${defaultName}-light"

    # “font.nomral.family” set in “__configSrc” by default.
    # Example value: "IosevkaTerm Nerd Font Mono"
    , font ? null
    }:
    assert builtins.isString defaultName;
    assert builtins.isString darkName;
    assert builtins.isString lightName;
    assert ! isNull font -> builtins.isString font;
    let
      default = dark;
      dark = buildExecutable darkName "dark" (buildConfig darkName __darkColorsConfigSrc font);
      light = buildExecutable lightName "light" (buildConfig lightName __lightColorsConfigSrc font);
    in
    default // { inherit default dark light; };
in
customize {} // {
  inherit customize;
  mainConfigSrc        = __configSrc;
  darkColorsConfigSrc  = __darkColorsConfigSrc;
  lightColorsConfigSrc = __lightColorsConfigSrc;
}
