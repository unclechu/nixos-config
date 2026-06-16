# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# My home audio system setup.
#
# Currently there is a pair main full-range bookshelf speakers
# and a pair of subwoofer boxes (stereo configuration).

let sources = import ../../nix/sources.nix; in

{ lib
, runCommand
, stdenvNoCC
, makeBinaryWrapper
, shellcheck
, coreutils
, bash
, gnused
, bc
, pulseaudio
, jack2
, jack-example-tools
, jalv
, calf
, lsp-plugins
, xmlstarlet
}:

let
  # These parameters are overridden from in the preset file(s).
  params = {
    # Main full-range speakers
    mains = {
      inDb = 0.0;
      outDb = -9.0;
      balanceIn = 0.0; # -1 .. +1
      balanceOut = 0.0; # -1 .. +1
    };

    # Subwoofers
    sub = {
      inDb = 0.0;
      outDb = 0.0;
      balanceIn = 0.0; # -1 .. +1
      # My current subs are different, so there is sensitivity mismatch
      balanceOut = 0.18; # -1 .. +1

      xOver = {
        freqHz = 85.0;
        slope = xOverSlopes.lr4_24db;
      };
    };
  };

  xOverSlopes = {
    lr2_12db = 1;
    lr4_24db = 2;
    lr8_48db = 3;
    lr12_72db = 4;
    lr16_96db = 5;
  };

  # Note that values from `params` are not always written to the associated paths as-is.
  # For example decibels are converted to gain coefficients.
  paramsPaths =
    let
      pluginPresetPath =
        name: plugin: "/rack/plugin[@instance-name='${name}']/preset[@plugin='${plugin}']";
    in
    {
      mains =
        let
          presetPath = pluginPresetPath "mains-stereo" "stereo";
        in {
          inDb = "${presetPath}/param[@name='level_in']/@value";
          outDb = "${presetPath}/param[@name='level_out']/@value";
          balanceIn = "${presetPath}/param[@name='balance_in']/@value"; # -1 .. +1
          balanceOut = "${presetPath}/param[@name='balance_out']/@value"; # -1 .. +1
        };

      sub =
        let
          presetPath = pluginPresetPath "sub-stereo" "stereo";
        in {
          inDb = "${presetPath}/param[@name='level_in']/@value";
          outDb = "${presetPath}/param[@name='level_out']/@value";
          balanceIn = "${presetPath}/param[@name='balance_in']/@value"; # -1 .. +1
          balanceOut = "${presetPath}/param[@name='balance_out']/@value"; # -1 .. +1

          xOver = {
            freqHz = "sf_1";
            slope = "frs_1";
          };
        };
    };

  # Executable dependencies map.
  # Executable name = Package where the executable comes from.
  executablesMap = {
    sleep = coreutils;
    dirname = coreutils;

    bash = bash;
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

  # Full paths to the executables.
  # Executable name = Full absolute path to the executable.
  executables =
    builtins.mapAttrs
      (n: v: "${executablesMap.${n}}/bin/${n}")
      executablesMap;

  # `executables` but with shell escaped paths.
  es = builtins.mapAttrs (n: v: lib.escapeShellArg v) executables;

  executablesCheckPhase =
    let
      executableFileCheck = x: "[[ -f ${x} || -r ${x} || -x ${x} ]]";
    in
      ''
        ${builtins.concatStringsSep "\n" (map (x: ''
          if ! ${executableFileCheck x}; then (set -o xtrace; ${executableFileCheck x}); fi
        '') (builtins.attrValues es))}
      '';

  # Convert decibels to gain coefficient
  dbToCoeff = value: assert builtins.isFloat value; ''(
    # 6 remainder digits precision is enough
    value=$(<<< ${lib.escapeShellArg "scale=6; e(l(10) * ${toString value} / 20)"} ${es.bc} -l)
    case "$value" in .*) value="0$value" ;; esac # add leading zero if missing
    printf %s "$value"
  )'';

  lspXOverPreset =
    let
      replaceValue = symbol: value:
        assert builtins.isFloat value || builtins.isInt value;
        ''(
          ${es.sed} '
            /lv2:symbol "${symbol}"/{
              n
              s/\(pset:value\) [0-9]\+\(\.[0-9]\+\)\?$/\1 ${toString value}/
            }
          '
        )'';
    in
    runCommand "home-audio-setup-lsp-lh-xover-jalv-preset" {} ''(
      set -o nounset; set -o pipefail
      mkdir -- "$out"
      cp -- ${lib.escapeShellArg "${presets/lsp-lh-xover-jalv/manifest.ttl}"} "$out/manifest.ttl"

      STATE_PRESET=$(<${lib.escapeShellArg "${presets/lsp-lh-xover-jalv/state.ttl}"})

      ${'' printf '%s\n' "$STATE_PRESET" ''
        } | ${let f = x: x.sub.xOver.freqHz; in replaceValue (f paramsPaths) (f params)
        } | ${let f = x: x.sub.xOver.slope; in replaceValue (f paramsPaths) (f params)
        } > "$out/state.ttl"
    )'';

  calfPreset =
    let
      replaceValue = path: value: ''(
        INPUT=$(</dev/stdin)

        CHECK_CMD=(
          ${es.xmlstarlet} sel
          -t -v ${lib.escapeShellArg "count(${path})"}
        )
        COUNT=$(<<<"$INPUT" "''${CHECK_CMD[@]}")
        if (( COUNT <= 0 )); then
          >&2 printf 'Could not find XPath “%s” in this XML: \n' "${lib.escapeShellArg path}" "$INPUT"
          exit 1
        fi

        VALUE=${
          # Considering string as a shell expression
          if builtins.isString value then "$( ${value} )" else toString value
        }

        UPDATE_CMD=(
          ${es.xmlstarlet} ed
          -u ${lib.escapeShellArg path}
          -v "$VALUE"
        )

        # set -o xtrace
        <<<"$INPUT" "''${UPDATE_CMD[@]}"
      )'';
    in
    runCommand "home-audio-setup-calfjackhost-preset.xml" {} ''(
      set -o nounset; set -o pipefail

      PRESET=$(
        <${lib.escapeShellArg "${presets/calfjackhost.xml}"
        # The file is actually compatible with 1.0 but `xmlstarlet`
        # is being noisy about unsupported 1.1 XML version.
        } ${es.sed} 's/xml version="1.1"/xml version="1.0"/'
      )

      ${'' printf %s "$PRESET" ''
        } | ${let f = x: x.mains.inDb; in replaceValue (f paramsPaths) (dbToCoeff (f params))
        } | ${let f = x: x.mains.outDb; in replaceValue (f paramsPaths) (dbToCoeff (f params))
        } | ${let f = x: x.mains.balanceIn; in replaceValue (f paramsPaths) (f params)
        } | ${let f = x: x.mains.balanceOut; in replaceValue (f paramsPaths) (f params)
        } | ${let f = x: x.sub.inDb; in replaceValue (f paramsPaths) (dbToCoeff (f params))
        } | ${let f = x: x.sub.outDb; in replaceValue (f paramsPaths) (dbToCoeff (f params))
        } | ${let f = x: x.sub.balanceIn; in replaceValue (f paramsPaths) (f params)
        } | ${let f = x: x.sub.balanceOut; in replaceValue (f paramsPaths) (f params)
        } > "$out"
    )'';

  getScriptDependencies =
    lib.flip lib.pipe [
      builtins.readFile
      (lib.splitString "\n")
      (builtins.foldl' (
        acc: line:
          if builtins.isAttrs acc then acc else
          if acc != null then (
            if builtins.isList acc then (
              let match = builtins.match "^>/dev/null type ([^[:space:]]+)$" line; in
              if isNull match then { result = acc; } else acc ++ [(builtins.elemAt match 0)]
            ) else acc
          ) else if line == "# Guard dependencies" then [] else acc
      ) null)
      (x: assert builtins.isAttrs x; x)
      (x: assert builtins.length x.result > 0; x.result)
    ];

  scriptDependenciesToDerivations =
    lib.flip lib.pipe [
      (map (x: executablesMap.${x}))
      lib.unique
    ];

  home-audio-lh-xover = stdenvNoCC.mkDerivation rec {
    name = "home-audio-lh-xover";
    src = ./home-audio-lh-xover.sh;

    nativeBuildInputs = [
      makeBinaryWrapper
      shellcheck
    ];

    dontUnpack = true;
    doCheck = true;

    checkPhase = ''
      runHook preCheck
      shellcheck -- "$src"
      ${executablesCheckPhase}
      runHook postCheck
    '';

    installPhase = ''
      runHook preInstall

      BIN_PATH="$out/bin/"${lib.escapeShellArg name}
      install -Dm755 -- "$src" "$BIN_PATH"

      CMD=(
        wrapProgram "$BIN_PATH"
        --prefix PATH : ${lib.makeBinPath (lib.pipe src [
          getScriptDependencies
          scriptDependenciesToDerivations
        ])}
        --set JALV_LSP_XOVER_PRESET ${lib.escapeShellArg lspXOverPreset}
        --set CALFJACKHOST_PRESET ${lib.escapeShellArg calfPreset}
      )
      "''${CMD[@]}"

      runHook postInstall
    '';
  };

  home-audio-setup = stdenvNoCC.mkDerivation rec {
    name = "home-audio-setup";
    src = ./home-audio-setup.sh;
    LH_XOVER_EXE = lib.escapeShellArg "${home-audio-lh-xover}/bin/home-audio-lh-xover";

    nativeBuildInputs = [
      makeBinaryWrapper
      shellcheck
    ];

    dontUnpack = true;
    doCheck = true;

    checkPhase = ''
      runHook preCheck
      shellcheck -- "$src"
      ${executablesCheckPhase}
      if ! [[ -f "$LH_XOVER_EXE" && -r "$LH_XOVER_EXE" && -x "$LH_XOVER_EXE" ]]; then (
        set -o xtrace; [[ -f "$LH_XOVER_EXE" && -r "$LH_XOVER_EXE" && -x "$LH_XOVER_EXE" ]]
      ) fi
      runHook postCheck
    '';

    installPhase = ''
      runHook preInstall

      BIN_PATH="$out/bin/"${lib.escapeShellArg name}
      install -Dm755 -- "$src" "$BIN_PATH"
      substituteInPlace "$BIN_PATH" --replace-fail "./home-audio-lh-xover.sh" "$LH_XOVER_EXE"

      CMD=(
        wrapProgram "$BIN_PATH"
        --prefix PATH : ${lib.makeBinPath (lib.pipe src [
          getScriptDependencies
          scriptDependenciesToDerivations
        ])}
      )
      "''${CMD[@]}"

      runHook postInstall
    '';
  };
in

{
  inherit
    params

    lspXOverPreset
    calfPreset

    home-audio-lh-xover
    home-audio-setup
    ;
}
