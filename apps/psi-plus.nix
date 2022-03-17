# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ fetchFromGitHub, psi-plus, libsForQt5 }:

(psi-plus.override { enablePsiMedia = true; }).overrideAttrs (srcAttrs: rec {
  version = "1.5.1615";

  src = fetchFromGitHub {
    owner = "psi-plus";
    repo = "psi-plus-snapshots";
    rev = version;
    sha256 = "04i7p57fqkxlc0mk69b5zvxqh2mnqsszdiv6pwd7an41d5a8jgv8";
  };

  buildInputs = srcAttrs.buildInputs ++ [
    libsForQt5.qtimageformats
  ];
})
