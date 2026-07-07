# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../../nix/sources.nix; in

{ pkgs ? import sources.nixpkgs-master {}

, lib ? pkgs.lib
, callPackage ? pkgs.callPackage
, runCommand ? pkgs.runCommand

, dbus ? pkgs.dbus
, pcre ? pkgs.pcre
, xdotool ? pkgs.xdotool
, xwininfo ? pkgs.xwininfo

# Nim packages
, nim-dbus-src ? sources.nim-dbus

, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}
, mk-nim-app ? callPackage ../../utils/nim/mk-nim-app.nix {}

# nix-shell arguments
, inNixShell ? false
, __nimLsp ? "nimlsp" # one of: `[null "nimlsp" "nimlangserver"]`

# Build options
, __srcFile ? ./invert_window_colors.nim
}:

let
  pkgs = null; # Prevent from using directly

  invert-window-colors = mk-nim-app {
    name = "invert-window-colors";
    src = __srcFile;
    extraSrcFiles = [
      nim-dbus-source
      ./nim.cfg
      ./app.nim
      ./ipc.nim
      ./types.nim
    ];
    buildInputs = [ dbus pcre ];
    lspForShell = __nimLsp;
    e = executable-dependencies {
      xdotool = xdotool;
      xwininfo = xwininfo;
    };
  };

  nim-dbus-source = runCommand "nim-dbus-source" {} ''
    ln -s -- ${lib.escapeShellArg "${nim-dbus-src}"} "$out"
  '';
in

(if inNixShell then invert-window-colors.shell else invert-window-colors) // {
  inherit invert-window-colors nim-dbus-source;
  inherit (invert-window-colors) shell;
}
