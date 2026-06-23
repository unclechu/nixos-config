# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# Polybar runner script wrapped into a Nix derivation.
#
# It helps to not care about the script location.
# Just rely on the presence of “run-polybar” in the “PATH”.

{ callPackage
, writeText
, bash
, polybar # Intended to be provided by “./polybar.nix”
, gnugrep
, coreutils
, inotify-tools
, diffutils

, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../../utils/mk-generic-script.nix {}
}:

let
  e = executable-dependencies {
    bash = bash;
    polybar = polybar;
    polybar-msg = polybar;
    inotifywait = inotify-tools;
    grep = gnugrep;
    cut = coreutils;
    diff = diffutils;
  };

  polybar-config-file =
    writeText "polybar-config-file" (builtins.readFile ./config.ini);

  run-polybar = mk-generic-script {
    name = "run-polybar";
    src = ./run-polybar.sh;
    inherit e;

    wrapProgramArgs = [
      "--set" "POLYBAR_CONFIG_FILE" polybar-config-file
    ];
  };
in

run-polybar // { inherit polybar-config-file; }
