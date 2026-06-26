# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
args@{ pkgs, lib, ... }:

let
  sources = import ../nix/sources.nix;
  apps = (import ../my-packages.nix args).my-apps;
  terminal-emulators = import ../terminal-emulators.nix { inherit pkgs lib; };
  rofi-commands = import ./rofi-commands.nix { inherit pkgs lib; };

  i3-config = pkgs.callPackage sources.i3rc rec {
    autostartScript = lib.getExe apps.autostart-setup;

    scriptsPaths = {
      "autostart.sh" = autostartScript;
      "input.sh" = lib.getExe apps.input-setup;
      "cursor-to-display.pl" = lib.getExe apps.cursor-to-display;
      "gpaste-gui.pl" = lib.getExe apps.gpaste-gui;
      "pamng.sh" = lib.getExe apps.pamng;
      "screen-backlight.sh" = lib.getExe apps.screen-backlight;
      "invert-window-colors" = lib.getExe apps.invert-window-colors;
    };

    terminalDark = lib.getExe
      terminal-emulators.allTerminalEmulators."alacritty-jetbrains-font-dark";
    terminalLight = lib.getExe
      terminal-emulators.allTerminalEmulators."alacritty-jetbrains-font-light";

    runDark = rofi-commands.run.dark;
    runLight = rofi-commands.run.light;
    drunDark = rofi-commands.drun.dark;
    drunLight = rofi-commands.drun.light;
    selectWindowDark = rofi-commands.window.dark;
    selectWindowLight = rofi-commands.window.light;
  };
in

{
  services.xserver.windowManager.i3 = {
    enable = true;
    configFile = i3-config;
  };
}
