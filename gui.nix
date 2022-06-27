# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
args@{ pkgs, lib, config, ... }:
let
  sources = import nix/sources.nix;
  inherit (import ./constants.nix) xkb keyRepeat wenzelUserName;
  esc = lib.escapeShellArg;

  alacritty-config = pkgs.callPackage apps/alacritty {};
  terminal-emulators = import ./terminal-emulators.nix { inherit pkgs lib; };

  i3-config =
    let
      apps = (import ./my-packages.nix args).my-apps;
      exe = app: "${app}/bin/${lib.getName app}";

      rofiTheme = {
        dark  = "gruvbox-dark";
        light = "gruvbox-light-soft";
      };
    in
    pkgs.callPackage sources.i3rc rec {
      autostartScript = exe apps.autostart-setup;

      scriptsPaths = {
        "autostart.sh"         = autostartScript;
        "input.sh"             = exe apps.input-setup;
        "cursor-to-display.pl" = exe apps.cursor-to-display;
        "gpaste-gui.pl"        = exe apps.gpaste-gui;
        "pamng.sh"             = exe apps.pamng;
        "screen-backlight.sh"  = exe apps.screen-backlight;
        "invert-window-colors" = exe apps.invert-window-colors;
      };

      terminalDark  = exe terminal-emulators.allTerminalEmulators."alacritty-jetbrains-font-dark";
      terminalLight = exe terminal-emulators.allTerminalEmulators."alacritty-jetbrains-font-light";

      runDark   = "${esc (exe pkgs.rofi)} -show run  -theme ${esc rofiTheme.dark}";
      runLight  = "${esc (exe pkgs.rofi)} -show run  -theme ${esc rofiTheme.light}";
      drunDark  = "${esc (exe pkgs.rofi)} -show drun -theme ${esc rofiTheme.dark}";
      drunLight = "${esc (exe pkgs.rofi)} -show drun -theme ${esc rofiTheme.light}";

      selectWindowDark  = "${esc (exe pkgs.rofi)} -show window -theme ${esc rofiTheme.dark}";
      selectWindowLight = "${esc (exe pkgs.rofi)} -show window -theme ${esc rofiTheme.light}";
    };
in
{
  environment = {
    variables = {
      # XCompose and XIM setup
      XMODIFIERS = "@im=none";
      QT_IM_MODULE = "xim";
      GTK_IM_MODULE = "xim";
    };

    sessionVariables = {
      GTK_THEME = "Adwaita:dark";
    };

    etc = {
      "xdg/gtk-2.0/gtkrc".text = ''
        gtk-theme-name = "Adwaita-dark"
        gtk-icon-theme-name = "Adwaita"
      '';

      "xdg/gtk-3.0/settings.ini".text = ''
        [Settings]
        gtk-theme-name = Adwaita-dark
        gtk-application-prefer-dark-theme = true
        gtk-icon-theme-name = Adwaita
      '';

      # Qt4
      "xdg/Trolltech.conf".text = ''
        [Qt]
        style=GTK+
      '';
    };
  };

  qt5 = {
    enable = true;
    platformTheme = "gnome";
    style = "adwaita-dark";
  };

  # X11
  services.xserver = {
    enable = true;
    layout = xkb.layout;
    xkbOptions = xkb.options;

    desktopManager = {
      # default = "none";
      xterm.enable = false;
    };

    displayManager = {
      defaultSession = "none+i3";

      lightdm = {
        enable = true;

        greeters.gtk = {
          cursorTheme =
            let x = config.home-manager.users.${wenzelUserName}.home.pointerCursor;
            in { inherit (x) name package size; };
        };
      };

      sessionCommands = ''
        ${esc pkgs.xorg.xset}/bin/xset r rate ${esc keyRepeat.delay} ${esc keyRepeat.interval}
      '';
    };

    windowManager.i3 = {
      enable     = true;
      configFile = i3-config;
    };

    libinput.enable = true; # touchpad
  };

  services.unclutter = {
    enable = false;
    timeout = 1;
  };
}
