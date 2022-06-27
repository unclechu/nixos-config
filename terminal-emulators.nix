# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ pkgs, lib }:
let
  sources = import nix/sources.nix;
  termite-config = pkgs.callPackage sources.termiterc {};
  alacritty-config = pkgs.callPackage apps/alacritty {};
in
rec {
  mkCustomFontTerminal = terminalConfig: defaultName: font:
    let extract = lib.attrVals ["default" "dark" "light"]; in
    extract terminalConfig ++
    extract (terminalConfig.customize {
      inherit defaultName font;
    });

  mkCustomFontTerminals = commandNameInfix: font:
    mkCustomFontTerminal termite-config "termite-${commandNameInfix}-font" font
    ++ mkCustomFontTerminal alacritty-config "alacritty-${commandNameInfix}-font" font;
}
