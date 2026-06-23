# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../../nix/sources.nix; in

{ lib
, callPackage
, writeTextFile
, coreutils
, xrandr
, gawk
, dunst
, procps
, systemd
, dash

, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../../utils/mk-generic-script.nix {}
}:

let
  e = executable-dependencies {
    xrandr = xrandr;
    dunstctl = dunst;
    pidof = procps;
    awk = gawk;
    rm = coreutils;
    cp = coreutils;
    systemctl = systemd;
    dash = dash;
  };

  scriptSrc = ./pseudo-primary-display.sh;

  pseudo-primary-display = mk-generic-script {
    name = "pseudo-primary-display";
    src = scriptSrc;
    inherit e;
  };

  getLineByPrefix = prefix:
    lib.pipe scriptSrc [
      builtins.readFile
      (lib.splitString "\n")
      (builtins.filter (line: lib.hasPrefix prefix line))
      # Must find exactly one line
      (x: assert builtins.length x == 1; x)
      builtins.head
    ];

  # Copy selected pseudo primary display number to `$XDG_RUNTIME_DIR` for faster access.
  copyToRuntimeScript = writeTextFile rec {
    name = "copy-pseudo-primary-display-selection-to-runtime";
    executable = true;
    destination = "/bin/${name}";
    inherit (e) checkPhase;

    text = ''
      #! ${e.b.dash}
      set -o errexit || exit; set -o nounset; set -o pipefail
      ${getLineByPrefix "DISPLAY_NUM_FILE="}
      ${getLineByPrefix "DISPLAY_NUM_RUNTIME_FILE="}
      set -o xtrace
      ${e.s.cp} -vf -- "$DISPLAY_NUM_FILE" "$DISPLAY_NUM_RUNTIME_FILE"
    '';
  };
in

pseudo-primary-display // { inherit copyToRuntimeScript; }
