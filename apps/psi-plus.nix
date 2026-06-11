# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{ psi-plus
, qt6Packages
}:

(psi-plus.override { enablePsiMedia = true; }).overrideAttrs (srcAttrs: rec {
  buildInputs = srcAttrs.buildInputs ++ [
    qt6Packages.qtimageformats
  ];
})
