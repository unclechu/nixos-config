# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ pkgs, ... }:
let
  inherit (import ../constants.nix) wenzelUserName;
in
{
  home-manager.users.${wenzelUserName} = {
    xsession.pointerCursor = {
      name = "Bibata-Original-Ice";
      package = pkgs.bibata-cursors;
      defaultCursor = "left_ptr";
      size = 24;
    };
  };
}
