# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ pkgs, ... }:
let
  inherit (import ../../constants.nix) wenzelUserName;
  left-handed-bibata-cursors = pkgs.callPackage ./left-handed-bibata-cursors.nix {};
in
{
  home-manager.users.${wenzelUserName} = {
    xsession.pointerCursor = {
      name = "Bibata-Original-Ice";
      package = left-handed-bibata-cursors;
      defaultCursor = "left_ptr";
      size = 24;
    };
  };
}
