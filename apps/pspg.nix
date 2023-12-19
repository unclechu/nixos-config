# “pspg” — a TUI pager for tabular data (e.g. can work as a CSV file viewer in the terminal).
#
# This module just adds some configuration to “pspg” executable via command-line arguments.
#
# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{ lib, symlinkJoin, makeWrapper, pspg }:

let
  theme = 16; # “Simple theme” (a dark theme, black and white palette)
in

symlinkJoin {
  name = "${lib.getName pspg}-pre-configured";
  nativeBuildInputs = [ makeWrapper ];
  paths = [ pspg ];
  postBuild = ''
    wrapProgram "$out/bin/pspg" --add-flags --style=${lib.escapeShellArg (toString theme)}
  '';
}
