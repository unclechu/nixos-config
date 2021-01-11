{ pkgs       ? import <nixpkgs> {}
, utils      ? import (import ../nix/sources.nix).nix-utils { inherit pkgs; }
, dzen-box   ? import ./dzen-box { inherit pkgs; }
, wenzels-i3 ? import ../apps/wenzels-i3.nix { inherit pkgs; }
}:
assert pkgs.lib.isDerivation dzen-box;
assert
  builtins.isPath wenzels-i3.rc ||
  pkgs.lib.isDerivation wenzels-i3.rc ||
  builtins.isString wenzels-i3.rc.outPath;
let
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile "${wenzels-i3.rc}/apps/${name}.sh";
  bash = "${pkgs.bash}/bin/bash";
  dzen-box-exe = "${dzen-box}/bin/dzen-box";
  pactl = "${pkgs.pulseaudio}/bin/pactl";
  pacmd = "${pkgs.pulseaudio}/bin/pacmd";
  awk = "${pkgs.gawk}/bin/awk";
  xargs = "${pkgs.findutils}/bin/xargs";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable bash}
    ${utils.shellCheckers.fileIsExecutable dzen-box-exe}
    ${utils.shellCheckers.fileIsExecutable pactl}
    ${utils.shellCheckers.fileIsExecutable pacmd}
    ${utils.shellCheckers.fileIsExecutable awk}
    ${utils.shellCheckers.fileIsExecutable xargs}
  '';
in
writeCheckedExecutable name checkPhase ''
  #! ${bash}
  set -e
  exec <&-

  PATH=${esc pkgs.pulseaudio}/bin:$PATH
  PATH=${esc pkgs.gawk}/bin:$PATH
  PATH=${esc pkgs.findutils}/bin:$PATH

  # guard dependencies
  >/dev/null which pactl
  >/dev/null which awk
  >/dev/null which xargs

  ${src}

  sinks=`${esc pacmd} list-sinks`
  re_in_between='([
  ]\s{1,}[^*
  ]+)+'
  re="^.*\sname: <$SINK>''${re_in_between}\s{2,}volume: ([^
  ]+)''${re_in_between}\s{2,}muted: (yes|no)
  .*$"
  if [[ $sinks =~ $re ]]; then
    re='^.* ([0-9]+)% .* ([0-9]+)% .*$'
    if [[ ''${BASH_REMATCH[4]} == yes ]]; then
      ${esc dzen-box-exe} MUTE lightblue
    elif [[ ''${BASH_REMATCH[2]} =~ $re ]]; then
      ${esc dzen-box-exe} $(( (''${BASH_REMATCH[1]} + ''${BASH_REMATCH[2]}) / 2 ))% lightblue
    fi
  fi
''
