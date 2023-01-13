# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../nix/sources.nix; in

{ libsForQt5, lib, mtxclient, coeurl, re2, nheko }:

let
  addRe2Dependency = x: x.overrideAttrs (old: { buildInputs = old.buildInputs ++ [ re2 ]; });

  newer = {
    mtxclient = lib.pipe mtxclient [
      addRe2Dependency

      # Pinned “coeurl” (as in the “io.github.NhekoReborn.Nheko.yaml” file)
      (x: x.override { coeurl = newer.coeurl; })

      # Pinned version of the sources of “mtxclient”
      (x: x.overrideAttrs (old: {
        version = sources.mtxclient.branch;
        src = sources.mtxclient;
      }))
    ];

    coeurl = coeurl.overrideAttrs (old: {
      version = sources.coeurl.version;
      src = sources.coeurl;
    });

    nheko = lib.pipe nheko [
      # Pinned version (some dependencies are pinned by the version of Nheko)
      (x: x.overrideAttrs (old: {
        version = sources.nheko.branch;
        src = sources.nheko;
      }))

      # Pinned “mtxclient” (as in the “io.github.NhekoReborn.Nheko.yaml” file)
      (x: x.override { mtxclient = newer.mtxclient; })

      # Pinned “coeurl” (as in the “io.github.NhekoReborn.Nheko.yaml” file)
      (x: x.override { coeurl = newer.coeurl; })

      addRe2Dependency
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
