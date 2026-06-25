# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let
  sources = import ../../nix/sources.nix;
  constants = import ../../constants.nix;
in


# Helpers
{ lib
, callPackage
, symlinkJoin

# Derivation dependencies
, bash
, picom
, procps
, coreutils

# Overridable dependencies
, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../../utils/mk-generic-script.nix {}

# Build options
, # System config (e.g. self-reference) to extract machine host name.
  # Set to “null” to use in Nix REPL.
  systemConfig

, systemProfile ? constants.systemProfile.default
}:

let
  # Hardware detection stuff
  hostName = systemConfig.networking.hostName or null;
  wenzel-silver-laptop = callPackage ../../hardware/wenzel-silver-laptop.nix {};

  esc = lib.escapeShellArg;

  conf = lib.fileset.toSource rec {
    root = ./conf;
    fileset = lib.fileset.fileFilter (f: f.hasExt "conf") root;
  };

  executablesMap = {
    sh = bash;
    sleep = coreutils;
    dirname = coreutils;
    pkill = procps;
    picom = picom;
  };

  e = executable-dependencies executablesMap;

  hardwareSpecificConfig =
    "${conf}/${
      # Enable “vsync” for machines that use “modesetting” driver. After
      # “intel” driver was removed in NixOS 24.11 release “modesetting” is
      # the one that is supposed to be used. But despite
      # “Option "TearFree" "true"” being set there is still tearing
      # everywhere with “modesetting”. So using Picom with “--vsync” is
      # the solution to fix the screen tearing for “modesetting”
      # videodriver. The “TearFree” was merged to X.Org “master” a couple
      # years ago or so but never released. Maybe I’ll try to build from
      # “master” to see if it works.
      if hostName == wenzel-silver-laptop.networking.hostName
      then "with-vsync.conf"
      else "without-vsync.conf"
    }";

  shAssertions = {
    readablePredicate = x: '' [[ -f ${esc x} && -r ${esc x} ]] '';
    check = predicate: x: '' if ! (${predicate x}); then (set -x; ${predicate x}) fi '';
  };

  no-picom = mk-generic-script {
    name = "no-picom";
    src = ./no-picom.sh;
    inherit e;
  };

  eFinal = executable-dependencies (executablesMap // {
    inherit no-picom;
  });

  run-picom = mk-generic-script {
    name = "run-picom";
    src = ./run-picom.sh;
    e = eFinal;

    wrapProgramArgs = [
      "--set" "DEFAULT_PICOM_CONFIG_FILE" hardwareSpecificConfig
      "--set" "NO_PICOM_SCRIPT_EXE" "${eFinal.b.no-picom}"
    ] ++ lib.optionals (
      # Picom does not work with `DRI_PRIME=1`:
      #
      #   [ 06/26/2026 00:50:33.902 glx_init ERROR ] GLX_EXT_texture_from_pixmap is not supported by your driver
      #   [ 06/26/2026 00:50:33.902 initialize_backend FATAL ERROR ] Failed to initialize backend, aborting...
      #   [ 06/26/2026 00:50:33.902 draw_callback_impl FATAL ERROR ] Pre-render preparation has failed, exiting...
      #
      systemProfile == constants.systemProfile.graphics
    ) [
      "--set" "DRI_PRIME" "0"
    ];

    checkPhase = ''
      ${shAssertions.check shAssertions.readablePredicate hardwareSpecificConfig}
    '';
  };

  # My own Picom configuration and a couple of scripts to start and stop it
  wenzels-picom = symlinkJoin {
    name = "wenzels-picom";
    meta.mainProgram = run-picom.meta.mainProgram;
    paths = [ run-picom no-picom ];
  };
in

wenzels-picom
