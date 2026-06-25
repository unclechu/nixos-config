# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
args@{ pkgs, lib, ... }:

let
  esc = lib.escapeShellArg;
  inherit (pkgs) rofi;

  themes = {
    dark = "gruvbox-dark";
    light = "gruvbox-light-soft";
  };

  modes = [ "run" "drun" "window" ];
  cmd = mode: theme: "${esc (lib.getExe rofi)} -show ${esc mode} -theme ${esc themes.${theme}}";
in

lib.pipe modes [
  (map (mode: lib.nameValuePair mode null))
  builtins.listToAttrs
  (builtins.mapAttrs (mode: _: builtins.mapAttrs (theme: _: cmd mode theme) themes))
]
