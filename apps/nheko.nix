# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# See this file for pins of the dependencies for Nheko (mtxclient & coeurl):
# https://github.com/Nheko-Reborn/nheko/blob/master/io.github.NhekoReborn.Nheko.yaml

{ pkgs, lib, config, ... }:

let
  # This wrapper just disables OpenGL rendering for QML.
  # On one of my machines UI is flickering and glitching, with software rendering it’s fine.
  # Also I didn’t notice any lags when using software rendering.
  # The only downside is that you cannot play the videos (use external player instead).
  # See https://doc.qt.io/QtQuick2DRenderer/
  noGpuAccelerationWrapper = pkgs.symlinkJoin {
    name = "nheko";
    paths = [ pkgs.nheko ];
    nativeBuildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      wrapProgram "$out"/bin/nheko \
        --set QMLSCENE_DEVICE softwarecontext
    '';
  };

  wenzel-nixos-pc = pkgs.callPackage ../hardware/wenzel-nixos-pc.nix {};
  hostName = config.networking.hostName or null;
in

if hostName == wenzel-nixos-pc.networking.hostName
then noGpuAccelerationWrapper
else pkgs.nheko
