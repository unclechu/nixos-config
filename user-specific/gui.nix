# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ pkgs, ... }:
let
  inherit (import ../constants.nix) wenzelUserName;
  firefox = pkgs.callPackage ../apps/firefox.nix {};
in
{
  home-manager.users.${wenzelUserName} = {
    # For a list of available Kvantum themes see:
    # https://github.com/tsujan/Kvantum/tree/master/Kvantum/themes/kvthemes
    xdg.configFile."Kvantum/kvantum.kvconfig".text = ''
      [General]
      theme=Vivid-Dark-Kvantum
    '';
  };
}
