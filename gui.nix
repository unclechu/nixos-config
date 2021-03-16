let sources = import nix/sources.nix; in
{ pkgs, ... }:
let
  inherit (import ./constants.nix) xkb keyRepeat;
  nix-utils = pkgs.callPackage sources.nix-utils {};
  inherit (nix-utils) esc;
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
      lightdm.enable = true;

      sessionCommands = ''
        ${esc pkgs.xorg.xset}/bin/xset r rate ${esc keyRepeat.delay} ${esc keyRepeat.interval}
      '';
    };

    libinput.enable = true; # touchpad
  };
}
