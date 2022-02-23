# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ fetchFromGitHub, nheko, libsForQt5, mtxclient }:

let
  qt-jdenticon = libsForQt5.callPackage (
    { lib, fetchFromGitHub, mkDerivation, qmake, qtbase }:
    mkDerivation rec {
      pname = "qt-jdenticon";
      version = "0.2.1";

      src = fetchFromGitHub {
        owner = "Nheko-Reborn";
        repo = "qt-jdenticon";
        rev = "v${version}";
        sha256 = "1dk2cmz34pdn79k6lq6jlbvy0gfp0w2vv1w4f8c9xmgmf6brcxq1";
      };

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

  # A fix for Nheko crashing when connecting to matrix.org
  # matrix.org rolled out new Synapse with new version string format.
  # TODO: Remove when the update is in nixpkgs
  new-mtxclient = mtxclient.overrideAttrs (_: rec {
    version = "0.6.2";

    src = fetchFromGitHub {
      owner = "Nheko-Reborn";
      repo = "mtxclient";
      rev = "v${version}";
      sha256 = "118i9g7vz4xsgsm30r6wb473qigr3n0skz1qsdmrjh5hax4sihaf";
    };
  });
in

(nheko.override { mtxclient = new-mtxclient; }).overrideAttrs (srcAttrs: {
  buildInputs = srcAttrs.buildInputs ++ [
    libsForQt5.qtimageformats
    qt-jdenticon
  ];
})
