# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../nix/sources.nix; in

{ nheko, libsForQt5, lib, gst_all_1 }:

let
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

  # See https://github.com/NixOS/nixpkgs/issues/176482
  # TODO Remove after https://github.com/NixOS/nixpkgs/pull/176345 is released for 22.05
  fixBrokenQtSupportForGstPluginsGood = drv: drv.overrideAttrs (srcAttrs: {
    buildInputs =
      let
        gst-plugins-good = gst_all_1.gst-plugins-good.overrideAttrs (srcAttrs: {
          nativeBuildInputs = srcAttrs.nativeBuildInputs ++ [ libsForQt5.qtbase ];
        });
      in
        builtins.filter (x: lib.getName x != "gst-plugins-good") srcAttrs.buildInputs
        ++ [ gst-plugins-good ];
  });

  addMoreImageFormats = drv: drv.overrideAttrs (srcAttrs: {
    buildInputs = srcAttrs.buildInputs ++ [ libsForQt5.qtimageformats ];
  });

  addIdenticonsSupport = drv: drv.overrideAttrs (srcAttrs: {
    buildInputs = srcAttrs.buildInputs ++ [ qt-jdenticon ];
  });
in

lib.pipe nheko [
  addMoreImageFormats
  addIdenticonsSupport
  fixBrokenQtSupportForGstPluginsGood
]
