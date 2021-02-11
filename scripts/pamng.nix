let sources = import ../nix/sources.nix; in
{ pkgs     ? import <nixpkgs> {}
, utils    ? import sources.nix-utils { inherit pkgs; }
, dzen-box ? import ./dzen-box { inherit pkgs; }
, pamng    ? import "${sources.i3rc}/nix/apps/pamng.nix" { inherit pkgs; }
}:
assert pkgs.lib.isDerivation dzen-box;
assert pkgs.lib.isDerivation pamng;
let
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  bash = "${pkgs.bash}/bin/bash";
  pacmd = "${pkgs.pulseaudio}/bin/pacmd";
  dzen-box-exe = "${dzen-box}/bin/dzen-box";
  pamng-exe = "${pamng}/bin/pamng";
  grep = "${pkgs.gnugrep}/bin/grep";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable bash}
    ${utils.shellCheckers.fileIsExecutable pacmd}
    ${utils.shellCheckers.fileIsExecutable dzen-box-exe}
    ${utils.shellCheckers.fileIsExecutable pamng-exe}
    ${utils.shellCheckers.fileIsExecutable grep}
  '';
in
writeCheckedExecutable name checkPhase ''
  #! ${bash}
  set -eu
  exec <&-

  ${builtins.readFile pamng-exe}

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
      ${esc dzen-box-exe} MUTE lightblue
    elif [[ ''${BASH_REMATCH[2]} =~ $re ]]; then
      ${esc dzen-box-exe} $(( (BASH_REMATCH[1] + BASH_REMATCH[2]) / 2 ))% lightblue
    fi
  fi
''
