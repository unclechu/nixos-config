# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ pkgs, lib }:
let
  inherit (import ./constants.nix) wenzelUserName;
  sources = import nix/sources.nix;
  termite-config = pkgs.callPackage sources.termiterc {};
  alacritty-config = pkgs.callPackage apps/alacritty {};

  inherit (pkgs.callPackage scripts/tmuxed-alacritty {})
    tmuxed-alacritty-new
    tmuxed-alacritty-attach
    tmuxed-alacritty-nuke
    tmuxed-alacritty-new-prompt
    ;

  mkCustomFontTerminal = terminalConfig: defaultName: font:
    let
      # attrset → [derivation]
      extract = lib.attrVals ["default" "dark" "light"];

      # [derivation]
      terminals =
        extract terminalConfig ++
        extract (terminalConfig.customize {
          inherit defaultName font;
        });
    in
      builtins.foldl' (acc: x: acc ++ mkTmuxedAlacritty x) [] terminals ++
      terminals;

  # derivation → [derivation]
  # If “alacritty” is not part of the name an empty list is returned.
  mkTmuxedAlacritty = alacritty:
    if builtins.match ".*(alacritty).*" (lib.getName alacritty) != ["alacritty"]
      then []
      else let new = (tmuxed-alacritty-new alacritty); in [
        new
        (tmuxed-alacritty-attach alacritty)
        (tmuxed-alacritty-nuke alacritty)
        (tmuxed-alacritty-new-prompt {
          TMUXED_ALACRITTY_EXE = "${new}/bin/${lib.getName new}";
        } alacritty)
      ];

  mkCustomFontTerminals = commandNameInfix: font:
    mkCustomFontTerminal termite-config "termite-${commandNameInfix}-font" font
    ++ mkCustomFontTerminal alacritty-config "alacritty-${commandNameInfix}-font" font;

  # All terminal emulators with all configs for them (different color schemes and fonts)
  allTerminalEmulators = []
    ++ mkCustomFontTerminals "hack" "Hack"
    ++ mkCustomFontTerminals "ibm" "IBM Plex Mono"
    ++ mkCustomFontTerminals "iosevka" "IosevkaTerm Nerd Font"
    ++ mkCustomFontTerminals "jetbrains" "JetBrains Mono";

  configuration = {
    users.users.${wenzelUserName}.packages = allTerminalEmulators;
  };
in
{
  inherit configuration;

  allTerminalEmulators =
    builtins.listToAttrs (map (x: { name = lib.getName x; value = x; }) allTerminalEmulators);
}
