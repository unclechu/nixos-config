# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../../nix/sources.nix; in

{ lib
, callPackage
, symlinkJoin
, makeWrapper
# , libsForQt5
, qt6Packages
, qt6
, stdenv
}:

let
  # callQtPackage = libsForQt5.callPackage; # For Nheko 0.11.*
  callQtPackage = qt6.callPackage; # For Nheko 0.12.*

  # Nheko 0.12.* configuration from “nixos-unstable”.
  # In stable nixpkgs it’s still 0.11.*.
  v0_12 = rec {
    nheko = qt6Packages.callPackage ./nheko.nix { inherit mtxclient coeurl; };
    mtxclient = callPackage ./mtxclient.nix { inherit coeurl; };
    coeurl = callPackage ./coeurl.nix {};
  };

  addIdenticonsSupport = drv: drv.overrideAttrs (srcAttrs: {
    buildInputs = srcAttrs.buildInputs ++ [ qt-jdenticon ];
  });

  qt-jdenticon = callQtPackage (
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

lib.pipe v0_12.nheko [
  addIdenticonsSupport
  fixMissingKvantum
]
