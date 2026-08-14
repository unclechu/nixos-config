# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# My home audio system setup.
#
# Currently there is a pair main full-range bookshelf speakers
# and a pair of subwoofer boxes (stereo configuration).

{ pkgs ? import <nixpkgs> {}

, lib ? pkgs.lib
, callPackage ? pkgs.callPackage
, runCommand ? pkgs.runCommand
, coreutils ? pkgs.coreutils
, gnused ? pkgs.gnused
, bc ? pkgs.bc
, pulseaudio ? pkgs.pulseaudio
, jack2 ? pkgs.jack2
, jack-example-tools ? pkgs.jack-example-tools
, jalv ? pkgs.jalv
, calf ? pkgs.calf
, lsp-plugins ? pkgs.lsp-plugins # TODO: Add to LV_PATH
, xmlstarlet ? pkgs.xmlstarlet

, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../../utils/mk-generic-script.nix {}
}:

let
  pkgs = null;

  # These parameters are overridden from in the preset file(s).
  setups = lib.fix (self: {
    # LH for Low-High (subs + mains DSP crossover).
    # Typically used for bookshelves + subs.
    lh = {
      eq.parametricBands = [];

      # Subwoofers
      sub = defaultParams // {
        xOver = { freqHz = 100; slope = xOverSlopes.lr16_96db; };
      };

      # Main full-range speakers
      mid = defaultParams;

      hi-mid = null;
      hi = null;
    };

    # LH but for Rockville 64B/4Ω 6.5" bookshelves (pretty low sensitivity).
    lh-rv = self.lh // {
      sub = self.lh.sub // { outDb = -9.0; };
    };

    # LMH for Low-Mid-High (subs + mid-range + tweeter DSP crossover).
    lmh = {
      eq.parametricBands = [
        { f = 2269.14; l = -3.6; q = 2.089; }
        { f = 3374.3; l = -4.7; q = 1.0; }
      ];

      # Subwoofers
      #
      # - Drivers: B&C 18TBX100 4Ω (97dB) 18"
      # - Enclosure: the box PA 18 ECO MKII
      # - Amplifier: 2x Fosi Audio V3 Mono
      #
      inherit (self.lh) sub;

      # Mid-range speakers
      #
      # - Drivers: Visaton W 200 S 8Ω (88dB 1W/1m) 8"
      # - Enclosure: Four Connect 4-AI8S 8"
      # - Amplifier: 2x Fosi Audio V3 Mono
      #
      mid = defaultParams // {
        # Everything above goes to the tweeters
        xOver = { freqHz = 2000; slope = xOverSlopes.lr4_24db; };
      };

      hi-mid = null;

      # Tweeters
      #
      # - Drivers: Visaton G 25 FFL 8Ω (90dB 1W/1m)
      # - Waveguide: Visaton Waveguide WG 220x150
      # - Amplifier: Nobsound NS-04G PRO
      #
      hi = defaultParams;
    };

    # LMMH for Low-Mid-HiMid-High
    # (subs + mid-range + hi-mid-range + tweeter DSP crossover).
    lmmh = self.lmh // {
      eq.parametricBands = [
        { f = 6459.33; l = -2.0; q = 4.966; }
        { f = 7248.51; l = -1.0; q = 8.181; }
      ];

      sub = self.lmh.sub // {
        outDb = -6.0;
      };

      mid = self.lmh.mid // {
        outDb = -8.7;

        # Above frequencies go to split between hi-mid drivers and tweeters
        xOver = { freqHz = 800; slope = xOverSlopes.lr12_72db; };
      };

      # Hi-Mid-range speakers
      #
      # - Drivers: Visaton W 100 S 8Ω (86dB 1W/1m) 4"
      # - Enclosure: Some cheap small wooden boxes with sealed port hole
      # - Amplifier: Nobsound NS-04G PRO
      #
      hi-mid = defaultParams // {
        # Everything above goes to the tweeters
        xOver = { freqHz = 5000; slope = xOverSlopes.lr12_72db; };
      };

      hi = self.lmh.hi // {
        # “hi-mid” and “hi” use the same amplifier with the same gain setting.
        # And the tweeter is 4dB more efficient. Compensating for that here.
        inDb = -4.0;

        outDb = -0.7;
      };
    };
  });

  orderedRanges = ["sub" "mid" "hi-mid" "hi"];

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

  # For range channels volume and balance control.
  #
  # Note that values from `setups` are not always written to the associated paths as-is.
  # For example decibels are converted to gain coefficients.
  getCalfStereoPresetPaths = rangeName:
    let
      pluginName = "${rangeName}-stereo";
      pluginPath = "/rack/plugin[@instance-name='${pluginName}']";
      pluginPresetPath = "${pluginPath}/preset[@plugin='stereo']";
    in {
      inherit pluginPath;
      inDb = "${pluginPresetPath}/param[@name='level_in']/@value";
      outDb = "${pluginPresetPath}/param[@name='level_out']/@value";
      balanceIn = "${pluginPresetPath}/param[@name='balance_in']/@value"; # -1 .. +1
      balanceOut = "${pluginPresetPath}/param[@name='balance_out']/@value"; # -1 .. +1
    };

  calfEqPresetPaths =
    let
      pluginName = "eq";
      pluginPath = "/rack/plugin[@instance-name='${pluginName}']";
      pluginPresetPath = "${pluginPath}/preset[@plugin='eq12']";
    in {
      parametricBands = lib.pipe (lib.range 1 8) [
        (map (n: {
          name = "band${toString n}";
          value =
            let
              paramPath = name: "${pluginPresetPath}/param[@name='p${toString n}_${name}']/@value";
            in
              {
                active = paramPath "active"; # 0 or 1 integer
                freq = paramPath "freq"; # Hz
                level = paramPath "level"; # Gain coefficient (needs conversion from decibels)
                q = paramPath "q"; # Floating point number
              };
        }))
        builtins.listToAttrs
      ];
    };

  # For cross-over frequencies and slopes control
  getLspPresetXOverPaths = rangeNum:
    assert builtins.isInt rangeNum && rangeNum >= 1 && rangeNum <= 7;
    { freqHz = "sf_${toString rangeNum}"; slope = "frs_${toString rangeNum}"; };

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
    assert builtins.elem setupTarget (builtins.attrNames setups);
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

      setup = let x = setups.${setupTarget}; in assert builtins.isAttrs x; x;

      # Type ∷ string
      initialReset = lib.pipe (lib.range 1 7) [
        (map (rangeNum:
          let paths = getLspPresetXOverPaths rangeNum; in
          [ (replaceValue paths.freqHz 10.0) (replaceValue paths.slope 0.0) ]
        ))
        lib.flatten
        (builtins.concatStringsSep " | ")
      ];

      # Type ∷ string
      replaces = lib.pipe orderedRanges [
        (builtins.foldl' (acc: rangeName:
          let rangeSetup = setup.${rangeName}; in
          if isNull rangeSetup || !(builtins.hasAttr "xOver" rangeSetup) then acc else {
            nextN = acc.nextN + 1;
            replaces =
              let paths = getLspPresetXOverPaths acc.nextN; in
              acc.replaces ++ [
                (replaceValue paths.freqHz rangeSetup.xOver.freqHz)
                (replaceValue paths.slope rangeSetup.xOver.slope)
              ];
          }
        ) { nextN = 1; replaces = []; })
        (x: x.replaces)
        (builtins.concatStringsSep " | ")
      ];
    in
    runCommand "home-audio-setup-lsp-xover-jalv-preset-${setupTarget}" {} ''
      set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
      mkdir -- "$out"
      cp -- ${lib.escapeShellArg "${presets/lsp-xover-jalv/manifest.ttl}"} "$out/manifest.ttl"
      STATE_PRESET=$(<${lib.escapeShellArg "${presets/lsp-xover-jalv/state.ttl}"})
      ${lib.pipe [
        '' printf '%s\n' "$STATE_PRESET" ''
        initialReset
        replaces
      ] [
        (builtins.filter (x: x != ""))
        (builtins.concatStringsSep " | ")
      ]} > "$out/state.ttl"
    '';

  mk-calfjackhost-preset = setupTarget:
    assert builtins.elem setupTarget (builtins.attrNames setups);
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

      removePlugin = pluginPath: ''(
        INPUT=$(</dev/stdin)
        ${checkPathExistence pluginPath}

        # set -o xtrace
        <<<"$INPUT" ${e.s.xmlstarlet} ed -d ${lib.escapeShellArg pluginPath}
      )'';

      setup = let x = setups.${setupTarget}; in assert builtins.isAttrs x; x;

      # Type ∷ string
      replaces = lib.pipe orderedRanges [
        # Calf Stereo levels and balances
        (map (rangeName:
          let
            rangeSetup = setup.${rangeName};
            stereoPaths = getCalfStereoPresetPaths rangeName;
          in
          if isNull rangeSetup then [
            (removePlugin (getCalfStereoPresetPaths rangeName).pluginPath)
          ] else [
            (let f = x: x.inDb; in replaceValue (f stereoPaths) (dbToCoeff (f rangeSetup)))
            (let f = x: x.outDb; in replaceValue (f stereoPaths) (dbToCoeff (f rangeSetup)))
            (let f = x: x.balanceIn; in replaceValue (f stereoPaths) (f rangeSetup))
            (let f = x: x.balanceOut; in replaceValue (f stereoPaths) (f rangeSetup))
          ]
        ))

        lib.flatten

        # Reset (disable) all EQ parametric bands first
        (x: x ++ (
          lib.pipe calfEqPresetPaths.parametricBands [
            builtins.attrValues
            (map (paths: replaceValue paths.active 0))
          ]
        ))

        # Configure EQ parametric bands according to the setup EQ configuration
        (x: x ++ lib.pipe setup.eq.parametricBands [
          (builtins.foldl' (acc: band: {
            nextBandN = acc.nextBandN + 1;
            replaces =
              let paths = calfEqPresetPaths.parametricBands."band${toString acc.nextBandN}"; in
              acc.replaces ++ [
                (replaceValue paths.active 1)
                (replaceValue paths.freq band.f)
                (replaceValue paths.level (dbToCoeff band.l))
                (replaceValue paths.q band.q)
              ];
          }) { nextBandN = 1; replaces = []; })
          (x: x.replaces)
        ])

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

      ${lib.pipe [
        '' printf %s "$PRESET" ''
        replaces
      ] [
        (builtins.filter (x: x != ""))
        (builtins.concatStringsSep " | ")
      ]} > "$out"
    '';

  presetMapBySetupTarget =
    builtins.foldl' (acc: setupTarget:
      acc // {
        ${setupTarget} = {
          lsp-xover = mk-lsp-xover-preset setupTarget;
          calfjackhost = mk-calfjackhost-preset setupTarget;
        };
      }
    ) {} (builtins.attrNames setups);

  mk-home-audio-xover-script = setupTarget:
    assert builtins.elem setupTarget (builtins.attrNames setups);
    mk-generic-script {
      name = "home-audio-xover-${setupTarget}";
      src = ./home-audio-xover.sh;
      inherit e;
      wrapProgramArgs = [
        "--add-flag" (lib.escapeShellArg setupTarget)
        "--set" "JALV_LSP_XOVER_PRESET" presetMapBySetupTarget.${setupTarget}.lsp-xover
        "--set" "CALFJACKHOST_PRESET" presetMapBySetupTarget.${setupTarget}.calfjackhost
      ];
    };

  home-audio-xover-mapBySetupTarget =
    builtins.foldl' (acc: setupTarget:
      acc // { "home-audio-xover-${setupTarget}" = mk-home-audio-xover-script setupTarget; }
    ) {} (builtins.attrNames setups);

  eFinal = executable-dependencies (executablesMap // home-audio-xover-mapBySetupTarget);

  home-audio-setup = mk-generic-script {
    name = "home-audio-setup";
    src = ./home-audio-setup.sh;
    e = eFinal;

    postPatch = ''
      CMD=(
        substituteInPlace "$src"
        --replace-fail './home-audio-xover.sh lh' ${eFinal.s.home-audio-xover-lh}
        --replace-fail './home-audio-xover.sh lmh' ${eFinal.s.home-audio-xover-lmh}
        --replace-fail './home-audio-xover.sh lmmh' ${eFinal.s.home-audio-xover-lmmh}
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
    setups
    home-audio-setup
    home-audio-mic
    ;
} // home-audio-xover-mapBySetupTarget // (
  builtins.foldl' (acc: setupTarget:
    let presets = presetMapBySetupTarget.${setupTarget}; in
    acc // {
      "lsp-xover-preset-${setupTarget}" = presets.lsp-xover;
      "calfjackhost-preset-${setupTarget}" = presets.calfjackhost;
    }
  ) {} (builtins.attrNames setups)
)
