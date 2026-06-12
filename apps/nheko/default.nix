# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../../nix/sources.nix; in

{ lib
, qt6
, stdenv
, nheko
, curl
, coeurl
, fetchurl
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
          --replace-fail "\$\$[QT_INSTALL_PLUGINS]" "$out/$qtPluginPrefix"
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

  # Temporary fix for https://github.com/Nheko-Reborn/nheko/issues/2054
  # See also https://github.com/curl/curl/issues/21547
  # TODO: Remove when Curl is updated in nixpkgs to a version with a fix.
  downgradeCurl = drv: drv.override {
    curl = olderCurl;
    coeurl = coeurl.override { curl = olderCurl; };
  };

  olderCurl =
    assert curl.version == "8.20.0";
    curl.overrideAttrs (old: rec {
      version = "8.19.0";
      src = fetchurl {
        urls = [
          "https://curl.haxx.se/download/curl-${version}.tar.xz"
          "https://github.com/curl/curl/releases/download/curl-${
            builtins.replaceStrings [ "." ] [ "_" ] version
          }/curl-${version}.tar.xz"
        ];
        hash = "sha256-TrQUiXkNGeGQ16x+GOgoV83Wivj05mspLO1WLTM/Ed8=";
      };
    });
in

lib.pipe nheko [
  downgradeCurl
  addIdenticonsSupport
]
