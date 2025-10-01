# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../../nix/sources.nix; in

{ lib
, qt6
, stdenv
, nheko
}:

let
  addIdenticonsSupport = drv: drv.overrideAttrs (srcAttrs: {
    buildInputs = srcAttrs.buildInputs ++ [ qt-jdenticon ];
  });

  qt-jdenticon = qt6.callPackage (
    # “mkDerivation” does not come with Qt6
    { lib, qmake, qtbase }:
    stdenv.mkDerivation rec {
      pname = sources.qt-jdenticon.repo;
      version = sources.qt-jdenticon.branch;
      src = sources.qt-jdenticon;

      nativeBuildInputs = [ qmake ];
      buildInputs = [ qtbase ];

      dontWrapQtApps = true;

      postPatch = ''
        # Fix plugins dir
        substituteInPlace QtIdenticon.pro \
          --replace "\$\$[QT_INSTALL_PLUGINS]" "$out/$qtPluginPrefix"
      '';

      meta = with lib; {
        description = "Qt plugin for generating highly recognizable identicons";
        homepage = "https://github.com/Nheko-Reborn/qt-jdenticon";
        license = licenses.mit;
        platforms = platforms.all;
        maintainers = with maintainers; [ unclechu ];
      };
    }
  ) {};
in

lib.pipe nheko [
  addIdenticonsSupport
]
