# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ callPackage
, lib
, pulseaudio

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}
, __dzen-box ? callPackage ./dzen-box { inherit __nix-utils; }
}:
let
  inherit (__nix-utils) esc shellCheckers;
  pacmd = "${pulseaudio}/bin/pacmd";
  dzen-box = "${__dzen-box}/bin/${__dzen-box.name}";

  pamng = callPackage "${sources.i3rc}/nix/apps/pamng.nix" {
    injectCheckPhase = ''
      ${shellCheckers.fileIsExecutable pacmd}
      ${shellCheckers.fileIsExecutable dzen-box}
    '';
    injectScriptPost = ''
      sinks=$(${esc pacmd} list-sinks)
      re_in_between='([
      ]	\s*[^
      ]+)+[
      ]	\s*'
      re="^.*[
      ]  \* index: [0-9]+[
      ].*\s+name: <$SINK>''${re_in_between}volume: ([^
      ]+)''${re_in_between}muted: (yes|no)[
      ].*$"
      if [[ $sinks =~ $re ]]; then
        re='^.* ([0-9]+)% .* ([0-9]+)% .*$'
        if [[ ''${BASH_REMATCH[4]} == yes ]]; then
          ${esc dzen-box} MUTE lightblue
        elif [[ ''${BASH_REMATCH[2]} =~ $re ]]; then
          ${esc dzen-box} $(( (BASH_REMATCH[1] + BASH_REMATCH[2]) / 2 ))% lightblue
        fi
      fi
    '';
  };
in
assert lib.isDerivation __dzen-box;
assert lib.isDerivation pamng;
pamng
