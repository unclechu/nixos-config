# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../../nix/sources.nix; in

# Helpers
{ callPackage
, runCommand
, lib

# Derivation dependencies
, bash
, picom
, procps
, coreutils

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}

# Build options
, # System config (e.g. self-reference) to extract machine host name.
  # Set to “null” to use in Nix REPL.
  systemConfig
}:

let
  # Hardware detection stuff
  hostName = systemConfig.networking.hostName or null;
  rw-wenzel-nixos-laptop = callPackage ../../hardware/rw-wenzel-nixos-laptop.nix {};
  wenzel-silver-laptop = callPackage ../../hardware/wenzel-silver-laptop.nix {};

  conf = lib.fileset.toSource rec {
    root = ./conf;
    fileset = lib.fileset.fileFilter (f: f.hasExt "conf") root;
  };

  bashExe = "${bash}/bin/bash";

  shAddDependencies = dependencies:
    builtins.concatStringsSep "\n" [
      "PATH=${lib.escapeShellArg (lib.makeBinPath dependencies)}\${PATH:+:}$PATH"
      "export PATH"
    ];

  # My own Picom configuration and a couple of scripts to start and stop it
  wenzels-picom = runCommand "wenzels-picom" {} ''
    mkdir --parents -- "$out"/bin

    [[ -x ${lib.escapeShellArg bashExe} ]] \
      || (set -o xtrace; [[ -x ${lib.escapeShellArg bashExe} ]])

    >"$out"/bin/run-picom printf '%s\n%s\n' ${
      lib.escapeShellArg ''
        #! ${bashExe}
        ${shAddDependencies [ coreutils picom ]}

        export DEFAULT_PICOM_CONFIG_FILE=${
          lib.escapeShellArg "${conf}/${
            # Enable “vsync” for machines that use “modesetting” driver. After
            # “intel” driver was removed in NixOS 24.11 release “modesetting” is
            # the one that is supposed to be used. But despite
            # “Option "TearFree" "true"” being set there is still tearing
            # everywhere with “modesetting”. So using Picom with “--vsync” is
            # the solution to fix the screen tearing for “modesetting”
            # videodriver. The “TearFree” was merged to X.Org “master” a couple
            # years ago or so but never released. Maybe I’ll try to build from
            # “master” to see if it works.
            if hostName == rw-wenzel-nixos-laptop.networking.hostName
            || hostName == wenzel-silver-laptop
            then "with-vsync.conf"
            else "without-vsync.conf"
          }"
        }
      ''
    } ${
      lib.escapeShellArg (builtins.readFile ./run-picom.sh)
    }

    >"$out"/bin/no-picom printf '%s\n%s\n' ${
      lib.escapeShellArg ''
        #! ${bashExe}
        ${shAddDependencies [ coreutils procps ]}
      ''
    } ${
      lib.escapeShellArg (builtins.readFile ./no-picom.sh)
    }

    chmod +x -- "$out"/bin/run-picom "$out"/bin/no-picom
  '';
in

wenzels-picom
