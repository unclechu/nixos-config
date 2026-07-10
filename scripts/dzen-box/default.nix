# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../../nix/sources.nix; in

{ pkgs ? import sources.nixpkgs-master {}

, callPackage ? pkgs.callPackage

, dzen2 ? pkgs.dzen2
, coreutils ? pkgs.coreutils
, util-linux ? pkgs.util-linux

, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}
, mk-nim-app ? callPackage ../../utils/nim/mk-nim-app.nix {}

# nix-shell arguments
, inNixShell ? false
, __nimLsp ? "nimlsp" # one of: `[null "nimlsp" "nimlangserver"]`

# Build options
, __srcFile ? ./dzen_box.nim
}:

let
  pkgs = null; # Prevent from using directly

  dzen-box = mk-nim-app {
    name = "dzen-box";
    src = __srcFile;
    extraSrcFiles = [
      ./config.nims
      ./needexe.nim
      ./stderr.nim
      ./subproc.nim
      ./poll.nim
      ../../utils/nim/log.nim
      ../../utils/nim/either.nim
      ../../utils/nim/lock.nim
      ../../utils/nim/signals.nim
      ../../utils/nim/timerfd.nim
    ];
    lspForShell = __nimLsp;
    e = executable-dependencies {
      env = coreutils;
      setsid = util-linux;
      dzen2 = dzen2;
    };
    wrapProgramArgs = [
      # By default log only fatal failures
      "--set-default" "DZEN_BOX_SILENT" "1"
    ];
  };
in

(if inNixShell then dzen-box.shell else dzen-box) // {
  inherit dzen-box;
  inherit (dzen-box) shell;
}
