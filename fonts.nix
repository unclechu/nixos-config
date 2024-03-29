# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ pkgs, ... }:
{
  console.font = "Lat2-Terminus16";

  fonts = {
    fontDir.enable = true;
    enableDefaultPackages = true;
    enableGhostscriptFonts = true;

    packages = [
      pkgs.hack-font
      pkgs.fira-code-symbols
      pkgs.iosevka
      pkgs.ibm-plex
      pkgs.nerdfonts
      pkgs.jetbrains-mono
    ];

    fontconfig = {
      antialias = true;
      hinting.enable = true;
      subpixel.rgba = "none";

      defaultFonts =
        let
          font = ["Hack"];
        in {
          monospace = font;
          sansSerif = font;
          serif = font;
          emoji = ["Noto Color Emoji"];
        };
    };
  };
}
