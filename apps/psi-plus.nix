# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../nix/sources.nix; in

{ psi-plus
, libsForQt5
, libomemo-c
}:

(psi-plus.override { enablePsiMedia = true; }).overrideAttrs (srcAttrs: rec {
  version = sources.psi-plus.branch;
  src = sources.psi-plus;

  buildInputs = srcAttrs.buildInputs ++ [
    libsForQt5.qtimageformats
    libsForQt5.qtkeychain
    libomemo-c
  ];
})
