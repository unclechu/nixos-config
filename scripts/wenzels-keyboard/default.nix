# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let
  constants = import ../../constants.nix;
  sources = import ../../nix/sources.nix;
in

{ pkgs ? import sources.nixpkgs-master {}

, callPackage ? pkgs.callPackage

, coreutils ? pkgs.coreutils
, util-linux ? pkgs.util-linux
, procps ? pkgs.procps
, xset ? pkgs.xset
, setxkbmap ? pkgs.setxkbmap
, numlockx ? pkgs.numlockx
, libnotify ? pkgs.libnotify

, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}
, mk-nim-app ? callPackage ../../utils/nim/mk-nim-app.nix {}
, __xbindkeys ? callPackage ../../apps/wenzels-xbindkeys.nix {}
, __wenzels-xlib-keys-hack ? callPackage ../../apps/wenzels-xlib-keys-hack {}

# nix-shell arguments
, inNixShell ? false
, __nimLsp ? "nimlsp" # one of: `[null "nimlsp" "nimlangserver"]`

# Build options
, __srcFile ? ./wenzels_keyboard.nim
, keyRepeatDelay ? constants.keyRepeat.delay
, keyRepeatInterval ? constants.keyRepeat.interval
, xkbLayout ? constants.xkb.layout
, xkbOptions ? constants.xkb.options
}:

assert builtins.isInt keyRepeatDelay && keyRepeatDelay >= 1;
assert builtins.isInt keyRepeatInterval && keyRepeatInterval >= 1;
assert builtins.isString xkbLayout && xkbLayout != "";
assert builtins.isString xkbOptions && xkbOptions != "";

let
  pkgs = null; # Prevent from using directly

  wenzels-keyboard = mk-nim-app {
    name = "wenzels-keyboard";
    src = __srcFile;
    extraSrcFiles = [
      ./config.nims
      ./needexe.nim
      ../../utils/nim/cliargs.nim
      ../../utils/nim/log.nim
    ];
    wrapProgramArgs = [
      "--add-flag" "--key-repeated-delay=${toString keyRepeatDelay}"
      "--add-flag" "--key-repeated-interval=${toString keyRepeatInterval}"
      "--add-flag" "--xkb-layout=${xkbLayout}"
      "--add-flag" "--xkb-options=${xkbOptions}"
    ];
    lspForShell = __nimLsp;
    e = executable-dependencies {
      env = coreutils;
      setsid = util-linux;
      pgrep = procps;
      xset = xset;
      setxkbmap = setxkbmap;
      numlockx = numlockx;
      notify-send = libnotify;
      xbindkeys = __xbindkeys;
      wenzels-xlib-keys-hack = __wenzels-xlib-keys-hack;
    };
  };
in

(if inNixShell then wenzels-keyboard.shell else wenzels-keyboard) // {
  inherit wenzels-keyboard;
  inherit (wenzels-keyboard) shell;
}
