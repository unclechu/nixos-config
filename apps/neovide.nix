# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{ lib, symlinkJoin, makeBinaryWrapper, neovide }:

symlinkJoin {
  name = "${lib.getName neovide}-fixed-scale-factor";
  nativeBuildInputs = [ makeBinaryWrapper ];
  paths = [ neovide ];
  postBuild = ''
    # Make the the font size scale predictably regardless of screen DPI.
    wrapProgram "$out/bin/neovide" --set WINIT_X11_SCALE_FACTOR 1.0
  '';
}
