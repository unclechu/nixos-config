args@{ pkgs, ... }:
let
  sources = import nix/sources.nix;
  inherit (import ./constants.nix) xkb keyRepeat;
  nix-utils = pkgs.callPackage sources.nix-utils {};
  inherit (nix-utils) esc;

  i3-config =
    let apps = (import ./my-packages.nix args).my-apps; in
    pkgs.callPackage sources.i3rc rec {
      autostartScript = let app = apps.autostart-setup; in "${app}/bin/${app.name}";

      scriptsPaths = {
        "autostart.sh"         = autostartScript;
        "input.sh"             = let app = apps.input-setup;          in "${app}/bin/${app.name}";
        "cursor-to-display.pl" = let app = apps.cursor-to-display;    in "${app}/bin/${app.name}";
        "gpaste-gui.pl"        = let app = apps.gpaste-gui;           in "${app}/bin/${app.name}";
        "pamng.sh"             = let app = apps.pamng;                in "${app}/bin/${app.name}";
        "screen-backlight.sh"  = let app = apps.screen-backlight;     in "${app}/bin/${app.name}";
        "invert-window-colors" = let app = apps.invert-window-colors; in "${app}/bin/${app.name}";
      };
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
      lightdm.enable = true;

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
}
