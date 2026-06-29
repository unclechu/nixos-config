# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
args@{ pkgs, lib, ... }:

let
  sources = import ../../nix/sources.nix;
  apps = (import ../../my-packages.nix args).my-apps;
  terminal-emulators = import ../../terminal-emulators.nix { inherit pkgs lib; };
  rofi-commands = import ../rofi-commands.nix { inherit pkgs lib; };

  executable-dependencies = pkgs.callPackage ../../utils/executable-dependencies.nix {};

  add-i3-pseudo-primary-display-runtime-config =
    pkgs.callPackage ./add-i3-pseudo-primary-display-runtime-config.nix {};

  e = executable-dependencies {
    add-i3-pseudo-primary-display-runtime-config =
      add-i3-pseudo-primary-display-runtime-config;
  };

  i3-config = pkgs.callPackage ./config.nix {
    autostart-setup = apps.autostart-setup;
    input-setup = apps.input-setup;
    gpaste-gui = apps.gpaste-gui;
    invert-window-colors = apps.invert-window-colors;
    pamng = apps.pamng;
    screen-backlight = apps.screen-backlight;

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

  wenzels-i3 = pkgs.symlinkJoin {
    name = "wenzels-i3";
    paths = [ pkgs.i3 ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      ${e.checkPhase}
      # Note that `--run` won’t re-run on i3 reload.
      # Make sure `pseudo-primary-display` runs it when changing pseudo primary.
      wrapProgram "$out"/bin/${
        lib.escapeShellArg pkgs.i3.meta.mainProgram
      } --run ${
        e.s.add-i3-pseudo-primary-display-runtime-config
      }
    '';
  };
in

{
  services.xserver.windowManager.i3 = {
    enable = true;
    configFile = i3-config;
    package = wenzels-i3;

    extraPackages = [
      (pkgs.callPackage ./wenzels-i3-status-generator {})
      e.executables.add-i3-pseudo-primary-display-runtime-config
      pkgs.i3status
      pkgs.i3lock
      pkgs.adwaita-icon-theme
    ];
  };
}
