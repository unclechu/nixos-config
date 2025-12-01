# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
args@{ pkgs, lib, config, ... }:
let
  sources = import nix/sources.nix;
  inherit (import ./constants.nix) xkb keyRepeat wenzelUserName;
  esc = lib.escapeShellArg;
in
{
  imports = [
    gui/xmonad
    gui/i3.nix
  ];

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

  qt = {
    enable = true;
    platformTheme = "qt5ct";
    style = "kvantum";
  };

  xdg.portal = {
    enable = true;
    extraPortals = [ pkgs.xdg-desktop-portal-gtk ];
    config.common = {
      default = [ "gtk" ];
    };
  };

  # X11
  services.xserver = {
    enable = true;
    inherit xkb;

    desktopManager = {
      # default = "none";
      xterm.enable = false;
    };

    displayManager = {
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
  };

  services.displayManager.defaultSession = "none+xmonad";

  services.libinput.enable = true; # touchpad

  services.unclutter = {
    enable = false;
    timeout = 1;
  };
}
