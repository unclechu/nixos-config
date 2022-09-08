# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../nix/sources.nix; in

{ libsForQt5, lib, mtxclient, nheko }:

let
  newer = {
    mtxclient = mtxclient.overrideAttrs (old: {
      version = sources.mtxclient.version;
      src = sources.mtxclient;
    });

    nheko = lib.pipe nheko [
      # Newer version
      (x: x.overrideAttrs (old: {
        version = sources.nheko.version;
        src = sources.nheko;
      }))

      # Use newer “mtxclient”
      (x: x.override { mtxclient = newer.mtxclient; })

      addIdenticonsSupport
    ];
  };

  addIdenticonsSupport = drv: drv.overrideAttrs (srcAttrs: {
    buildInputs = srcAttrs.buildInputs ++ [ qt-jdenticon ];
  });

  qt-jdenticon = libsForQt5.callPackage (
    { lib, mkDerivation, qmake, qtbase }:
    mkDerivation rec {
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

newer.nheko
