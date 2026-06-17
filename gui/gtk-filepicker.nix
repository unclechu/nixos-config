# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
args@{ pkgs, ... }:

let
  inherit (import ../constants.nix) wenzelUserName;
in

# Use Gtk file picker everywhere (including Qt apps)
{
  xdg.portal = {
    enable = true;

    extraPortals = [
      pkgs.xdg-desktop-portal-gtk
    ];

    config.common = {
      "org.freedesktop.impl.portal.FileChooser" = [ "gtk" ];
    };
  };

  home-manager.users.${wenzelUserName} = {
    xdg.configFile."qt5ct/qt5ct.conf".text = ''
      [Appearance]
      standard_dialogs=gtk3
    '';

    xdg.configFile."qt6ct/qt6ct.conf".text = ''
      [Appearance]
      standard_dialogs=gtk3
    '';
  };
}
