# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../../nix/sources.nix; in

{ lib
, symlinkJoin
, makeWrapper
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

  # Fix no UI at all in the Nheko window, only background.
  # Qt6 doesn’t seem to be properly set up for “kvantum” engine.
  fixMissingKvantum = drv: symlinkJoin {
    name = "${lib.getName drv}-missing-kvantum-fix";
    nativeBuildInputs = [ makeWrapper ];
    paths = [ drv ];
    postBuild = ''
      wrapProgram "$out"/bin/${lib.escapeShellArg (lib.getName drv)} --set QT_STYLE_OVERRIDE ""
    '';
  };
in

lib.pipe nheko [
  addIdenticonsSupport
  fixMissingKvantum
]
