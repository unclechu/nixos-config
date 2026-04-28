# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{ psi-plus
, libsForQt5
, libomemo-c
}:

(psi-plus.override { enablePsiMedia = true; }).overrideAttrs (srcAttrs: rec {
  buildInputs = srcAttrs.buildInputs ++ [
    libsForQt5.qtimageformats
    libsForQt5.qtkeychain
    libomemo-c
  ];
})
