# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# My home audio system setup.
#
# Currently there is a pair main full-range bookshelf speakers
# and a pair of subwoofer boxes (stereo configuration).

let sources = import ../../nix/sources.nix; in

{ lib
, callPackage
, runCommand
, coreutils
, gnused
, bc
, pulseaudio
, jack2
, jack-example-tools
, jalv
, calf
, lsp-plugins # TODO: Add to LV_PATH
, xmlstarlet

, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../../utils/mk-generic-script.nix {}
}:

let
  # These parameters are overridden from in the preset file(s).
  params = lib.fix (self: {
    # LH for Low-High (subs + mains DSP crossover)
    lh = {
      # Main full-range speakers
      mains = {
        inDb = 0.0;
        outDb = 0.0;
        balanceIn = 0.0; # -1 .. +1
        balanceOut = 0.0; # -1 .. +1
      };

      # Subwoofers
      sub = defaultParams // {
        xOver = { freqHz = 100; slope = xOverSlopes.lr16_96db; };
      };
    };

    # LMH for Low-Mid-High (subs + mid-range + tweeter DSP crossover)
    lmh = {
      # Subwoofers
      #
      # - Drivers: B&C 18TBX100 4Ω
      # - Enclosure: the box PA 18 ECO MKII
      #
      inherit (self.lh) sub;

      # Mid-range speakers
      #
      # - Drivers: Visaton W 200 S 8Ω
      # - Enclosure: Four Connect 4-AI8S 8"
      #
      mid = defaultParams // {
        # Everything above goes to the tweeters
        xOver = { freqHz = 2000; slope = xOverSlopes.lr8_48db; };
      };

      # Tweeters
      #
      # - Drivers: Visaton G 25 FFL 8Ω
      # - Waveguide: Visaton Waveguide WG 220x150
      #
      hi = defaultParams // { outDb = -0.7; };
    };
  });

  defaultParams = {
    inDb = 0.0;
    outDb = 0.0;
    balanceIn = 0.0; # -1 .. +1
    balanceOut = 0.0; # -1 .. +1
  };

  xOverSlopes = {
    lr2_12db = 1;
    lr4_24db = 2;
    lr8_48db = 3;
    lr12_72db = 4;
    lr16_96db = 5;
  };

  mkPluginPath = name: "/rack/plugin[@instance-name='${name}']";
  mkPluginPresetPath = name: plugin: "${mkPluginPath name}/preset[@plugin='${plugin}']";

  # Note that values from `params` are not always written to the associated paths as-is.
  # For example decibels are converted to gain coefficients.
  paramsPaths = lib.fix (self: {
    lh = {
      mains = defaultParamsPaths "mains-stereo" "stereo";
      sub = defaultParamsPaths "sub-stereo" "stereo" // {
        xOver = { freqHz = "sf_1"; slope = "frs_1"; };
      };
    };

    lmh = {
      inherit (self.lh) sub;
      mid = self.lh.mains // {
        renamePluginTo = "mid-stereo";
        xOver = { freqHz = "sf_2"; slope = "frs_2"; };
      };
      hi = defaultParamsPaths "hi-stereo" "stereo";
    };
  });

  defaultParamsPaths = pluginName: presetPlugin:
    let presetPath = mkPluginPresetPath pluginName presetPlugin; in {
      pluginPath = mkPluginPath pluginName;
      inDb = "${presetPath}/param[@name='level_in']/@value";
      outDb = "${presetPath}/param[@name='level_out']/@value";
      balanceIn = "${presetPath}/param[@name='balance_in']/@value"; # -1 .. +1
      balanceOut = "${presetPath}/param[@name='balance_out']/@value"; # -1 .. +1
    };

  executablesMap = {
    sleep = coreutils;
    dirname = coreutils;

    sed = gnused;
    bc = bc;
    pactl = pulseaudio;

    jack_control = jack2;
    jack_lsp = jack-example-tools;
    jack_disconnect = jack-example-tools;
    jack_connect = jack-example-tools;

    "jalv.gtk3" = jalv;
    calfjackhost = calf;
    xmlstarlet = xmlstarlet;
  };

  e = executable-dependencies executablesMap;

  # Convert decibels to gain coefficient
  dbToCoeff = value: assert builtins.isFloat value; ''(
    # 6 remainder digits precision is enough
    value=$(<<< ${lib.escapeShellArg "scale=6; e(l(10) * ${toString value} / 20)"} ${e.s.bc} -l)
    case "$value" in .*) value="0$value" ;; esac # add leading zero if missing
    printf %s "$value"
  )'';

  mk-lsp-xover-preset = setupTarget:
    assert builtins.elem setupTarget ["lh" "lmh"];
    let
      replaceValue = symbol: value:
        assert builtins.isFloat value || builtins.isInt value;
        ''(
          ${e.s.sed} '
            /lv2:symbol "${symbol}"/{
              n
              s/\(pset:value\) [0-9]\+\(\.[0-9]\+\)\?$/\1 ${toString value}/
            }
          '
        )'';

      params' = let x = params.${setupTarget}; in assert builtins.isAttrs x; x;
      paramsPaths' = let x = paramsPaths.${setupTarget}; in assert builtins.isAttrs x; x;

      defaultReplaces = lib.pipe params' [
        builtins.attrNames
        (builtins.filter (name: builtins.hasAttr "xOver" params'.${name}))
        (names: assert builtins.length names > 0; names)
        (map (name: [
          (let f = x: x.${name}.xOver.freqHz; in replaceValue (f paramsPaths') (f params'))
          (let f = x: x.${name}.xOver.slope; in replaceValue (f paramsPaths') (f params'))
        ]))
        lib.flatten
        (builtins.concatStringsSep " | ")
      ];
    in
    runCommand "home-audio-setup-lsp-xover-jalv-preset-${setupTarget}" {} ''
      set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
      mkdir -- "$out"
      cp -- ${lib.escapeShellArg "${presets/lsp-xover-jalv/manifest.ttl}"} "$out/manifest.ttl"
      STATE_PRESET=$(<${lib.escapeShellArg "${presets/lsp-xover-jalv/state.ttl}"})
      ${'' printf '%s\n' "$STATE_PRESET" ''} | ${defaultReplaces} > "$out/state.ttl"
    '';

  mk-calfjackhost-preset = setupTarget:
    assert builtins.elem setupTarget ["lh" "lmh"];
    let
      checkPathExistence = path: ''(
        [[ -v INPUT ]] # Must be defined
        CHECK_CMD=(
          ${e.s.xmlstarlet} sel
          -t -v ${lib.escapeShellArg "count(${path})"}
        )
        COUNT=$(<<<"$INPUT" "''${CHECK_CMD[@]}")
        if (( COUNT <= 0 )); then
          >&2 printf 'Could not find XPath “%s” in this XML: \n' "${lib.escapeShellArg path}" "$INPUT"
          exit 1
        fi
      )'';

      replaceValue = path: value: ''(
        INPUT=$(</dev/stdin)
        ${checkPathExistence path}

        VALUE=${
          # Considering string value a shell expression
          if builtins.isString value then "$( ${value} )" else toString value
        }

        UPDATE_CMD=(
          ${e.s.xmlstarlet} ed
          -u ${lib.escapeShellArg path}
          -v "$VALUE"
        )

        # set -o xtrace
        <<<"$INPUT" "''${UPDATE_CMD[@]}"
      )'';

      renamePlugin = pluginPath: newPluginName: ''(
        INPUT=$(</dev/stdin)
        ${checkPathExistence pluginPath}

        UPDATE_CMD=(
          ${e.s.xmlstarlet} ed
          -u ${lib.escapeShellArg "${pluginPath}/@instance-name"}
          -v ${lib.escapeShellArg newPluginName}
        )

        # set -o xtrace
        <<<"$INPUT" "''${UPDATE_CMD[@]}"
      )'';

      removePlugin = pluginPath: ''(
        INPUT=$(</dev/stdin)
        ${checkPathExistence pluginPath}

        # set -o xtrace
        <<<"$INPUT" ${e.s.xmlstarlet} ed -d ${lib.escapeShellArg pluginPath}
      )'';

      noOp = ''(X=$(</dev/stdin); printf '%s\n' "$X")'';

      params' = let x = params.${setupTarget}; in assert builtins.isAttrs x; x;
      paramsPaths' = let x = paramsPaths.${setupTarget}; in assert builtins.isAttrs x; x;

      mainsName =
        if setupTarget == "lh" then "mains"
        else if setupTarget == "lmh" then "mid"
        else throw "Unexpected `setupTarget` value: `${setupTarget}`"
      ;

      defaultReplaces = bandName: lib.pipe [
        (let f = x: x.${bandName}.inDb; in replaceValue (f paramsPaths') (dbToCoeff (f params')))
        (let f = x: x.${bandName}.outDb; in replaceValue (f paramsPaths') (dbToCoeff (f params')))
        (let f = x: x.${bandName}.balanceIn; in replaceValue (f paramsPaths') (f params'))
        (let f = x: x.${bandName}.balanceOut; in replaceValue (f paramsPaths') (f params'))
        (
          let x = paramsPaths'.${bandName}.renamePluginTo or null; in
          if isNull x then noOp else renamePlugin paramsPaths'.${bandName}.pluginPath x
        )
      ] [
        (builtins.concatStringsSep " | ")
      ];
    in
    runCommand "home-audio-setup-calfjackhost-preset-${setupTarget}.xml" {} ''
      set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail

      PRESET=$(
        <${lib.escapeShellArg "${presets/calfjackhost.xml}"
        # The file is actually compatible with 1.0 but `xmlstarlet`
        # is being noisy about unsupported 1.1 XML version.
        } ${e.s.sed} 's/xml version="1.1"/xml version="1.0"/'
      )

      ${'' printf %s "$PRESET" ''
        } | ${defaultReplaces "sub"
        } | ${defaultReplaces mainsName
        } | ${
          if setupTarget == "lh" then removePlugin paramsPaths.lmh.hi.pluginPath
          else if setupTarget == "lmh" then defaultReplaces "hi"
          else throw "Unexpected `setupTarget` value: `${setupTarget}`"
        } > "$out"
    '';

  lsp-xover-preset-lh = mk-lsp-xover-preset "lh";
  lsp-xover-preset-lmh = mk-lsp-xover-preset "lmh";

  calfjackhost-preset-lh = mk-calfjackhost-preset "lh";
  calfjackhost-preset-lmh = mk-calfjackhost-preset "lmh";

  presetMapBySetupTarget = {
    lh = {
      lsp-xover = lsp-xover-preset-lh;
      calfjackhost = calfjackhost-preset-lh;
    };
    lmh = {
      lsp-xover = lsp-xover-preset-lmh;
      calfjackhost = calfjackhost-preset-lmh;
    };
  };

  mk-home-audio-xover-script = setupTarget:
    assert builtins.elem setupTarget ["lh" "lmh"];
    mk-generic-script {
      name = "home-audio-xover-${setupTarget}";
      src = ./home-audio-xover.sh;
      inherit e;
      wrapProgramArgs = [
        "--add-flag" (lib.escapeShellArg setupTarget)
        "--set" "JALV_LSP_XOVER_PRESET" presetMapBySetupTarget.${setupTarget}.lsp-xover
        "--set" "CALFJACKHOST_PRESET" presetMapBySetupTarget.${setupTarget}.calfjackhost
      ] ++ lib.optionals (setupTarget == "lmh") [
        "--set" "MAINS_STEREO_NAME" (paramsPaths.${setupTarget}.mid.renamePluginTo)
      ];
    };

  home-audio-xover-lh = mk-home-audio-xover-script "lh";
  home-audio-xover-lmh = mk-home-audio-xover-script "lmh";

  eFinal = executable-dependencies (executablesMap // {
    home-audio-xover-lh = home-audio-xover-lh;
    home-audio-xover-lmh = home-audio-xover-lmh;
  });

  home-audio-setup = mk-generic-script {
    name = "home-audio-setup";
    src = ./home-audio-setup.sh;
    e = eFinal;

    postPatch = ''
      CMD=(
        substituteInPlace "$src"
        --replace-fail './home-audio-xover.sh lh' ${eFinal.s.home-audio-xover-lh}
        --replace-fail './home-audio-xover.sh lmh' ${eFinal.s.home-audio-xover-lmh}
      )
      "''${CMD[@]}"
    '';
  };

  home-audio-mic = mk-generic-script {
    name = "home-audio-mic";
    src = ./home-audio-mic.sh;
    e = eFinal;
    wrapProgramArgs = [ "--set" "CALFJACKHOST_PRESET" presets/calfjackhost-mic.xml ];
  };
in

{
  inherit
    params

    lsp-xover-preset-lh
    lsp-xover-preset-lmh

    calfjackhost-preset-lh
    calfjackhost-preset-lmh

    home-audio-xover-lh
    home-audio-xover-lmh

    home-audio-setup
    home-audio-mic
    ;
}
