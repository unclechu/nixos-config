{ pkgs, ... }:
{
  console.font = "Lat2-Terminus16";

  fonts = {
    enableFontDir = true;
    enableDefaultFonts = true;
    enableGhostscriptFonts = true;

    fonts = [
      pkgs.hack-font
      pkgs.fira-code-symbols
      pkgs.iosevka
      pkgs.ibm-plex
      pkgs.nerdfonts
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
