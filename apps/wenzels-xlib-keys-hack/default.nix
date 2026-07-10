# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../../nix/sources.nix; in

{ pkgs ? import sources.nixpkgs-master {}

, callPackage ? pkgs.callPackage

, coreutils ? pkgs.coreutils
, util-linux ? pkgs.util-linux
, dash ? pkgs.dash
, xinput ? pkgs.xinput
, gnused ? pkgs.gnused
, findutils ? pkgs.findutils
, libnotify ? pkgs.libnotify
, xdotool ? pkgs.xdotool

, __xlib-keys-hack ? callPackage sources.xlib-keys-hack {}

, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}
, mk-nim-app ? callPackage ../../utils/nim/mk-nim-app.nix {}

# nix-shell arguments
, inNixShell ? false
, __nimLsp ? "nimlsp" # one of: `[null "nimlsp" "nimlangserver"]`

# Build options
, __srcFile ? ./wenzels_xlib_keys_hack.nim
}:

let
  pkgs = null; # Prevent from using directly

  wenzels-xlib-keys-hack = mk-nim-app {
    name = "wenzels-xlib-keys-hack";
    src = __srcFile;
    extraSrcFiles = [
      ./config.nims
      ./needexe.nim
      ./stderr.nim
      ./subproc.nim
      ./poll.nim
      ./desktopnotifications.nim
      ../../utils/nim/cliargs.nim
      ../../utils/nim/log.nim
      ../../utils/nim/either.nim
      ../../utils/nim/signals.nim
    ];
    lspForShell = __nimLsp;
    e = executable-dependencies {
      xlib-keys-hack = __xlib-keys-hack;
      env = coreutils;
      setsid = util-linux;
      dash = dash;
      xinput = xinput;
      sed = gnused;
      xargs = findutils;
      sort = coreutils;
      notify-send = libnotify;
      xdotool = xdotool;
    };
  };
in

(if inNixShell then wenzels-xlib-keys-hack.shell else wenzels-xlib-keys-hack) // {
  inherit wenzels-xlib-keys-hack;
  inherit (wenzels-xlib-keys-hack) shell;
}
