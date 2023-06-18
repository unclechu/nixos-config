# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
args@{ pkgs, lib, ... }:

let
  sources = import ../nix/sources.nix;
  esc = lib.escapeShellArg;
  exe = app: "${app}/bin/${lib.getName app}";
  apps = (import ../my-packages.nix args).my-apps;
  terminal-emulators = import ../terminal-emulators.nix { inherit pkgs lib; };
  rofi-commands = import ./rofi-commands.nix { inherit pkgs lib; };

  i3-config = pkgs.callPackage sources.i3rc rec {
    autostartScript = exe apps.autostart-setup;

    scriptsPaths = {
      "autostart.sh" = autostartScript;
      "input.sh" = exe apps.input-setup;
      "cursor-to-display.pl" = exe apps.cursor-to-display;
      "gpaste-gui.pl" = exe apps.gpaste-gui;
      "pamng.sh" = exe apps.pamng;
      "screen-backlight.sh" = exe apps.screen-backlight;
      "invert-window-colors" = exe apps.invert-window-colors;
    };

    terminalDark = exe terminal-emulators.allTerminalEmulators."alacritty-jetbrains-font-dark";
    terminalLight = exe terminal-emulators.allTerminalEmulators."alacritty-jetbrains-font-light";

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
