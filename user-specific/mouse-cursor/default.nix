# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ pkgs, ... }:
let
  inherit (import ../../constants.nix) wenzelUserName;
in
{
  home-manager.users.${wenzelUserName} = {
    home.pointerCursor = {
      x11 = {
        enable = true;
        defaultCursor = "left_ptr";
      };

      name = "Bibata-Original-Ice-Right";
      package = pkgs.bibata-cursors;
      size = 24;
    };
  };
}
