# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ pkgs, lib, ... }:
let
  inherit (import ../constants.nix) wenzelUserName;
  firefox = pkgs.callPackage ../apps/firefox.nix {};
in
{
  home-manager.users.${wenzelUserName} = {
    services.dunst = {
      enable = true;
      settings = {
        global = {
          width = "300";
          offset = "20x20";
          notification_limit = "15";
          indicate_hidden = "yes";
          sort = "yes";
          word_wrap = "no";
          stack_duplicates = "yes";
          show_indicators = "yes";
          browser = lib.getExe firefox;
          transparency = "25";
          separator_height = "2";
          padding = "8";
          horizontal_padding = 8;
          frame_width = "8";
          frame_color = "#aaaaaa";
          separator_color = "frame";
          font = "Monospace 14";
          format = "<b>%s</b>\\n%b";
        };
        urgency_low = {
          background = "#222222";
          foreground = "#888888";
        };
        urgency_normal = {
          background = "#285577";
          foreground = "#ffffff";
        };
        urgency_critical = {
          background = "#900000";
          foreground = "#ffffff";
          frame_color = "#ff0000";
        };
      };
    };

    systemd.user.services.dunst.Service.ExecStart = lib.mkForce ''
      ${pkgs.writeShellScript "dunst-with-runtime-config" ''
        set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
        exec ${lib.escapeShellArg (lib.getExe pkgs.dunst)} -conf <(
          MAIN_CONF="$HOME/.config/dunst/dunstrc"
          config=$(<"$MAIN_CONF")
          printf '%s\n' "$config"

          DISPLAY_NUM_FILE=$HOME/.pseudo-primary-display
          if [[ -f "$DISPLAY_NUM_FILE" ]]; then
            DISPLAY_NUM=$(<"$DISPLAY_NUM_FILE")
            printf '[global]\nmonitor = %d\nfollow = none\n' "$(( DISPLAY_NUM - 1 ))"
          fi
        )
      ''}
    '';
  };
}
