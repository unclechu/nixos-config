# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ callPackage
, lib
, symlinkJoin
, makeBinaryWrapper
, firefox
}:
symlinkJoin rec {
  name = lib.getName firefox;
  meta.mainProgram = name;
  nativeBuildInputs = [ makeBinaryWrapper ];
  paths = [ firefox ];
  postBuild = ''
    wrapProgram "$out/bin/firefox" --set MOZ_USE_XINPUT2 1
  '';
}
