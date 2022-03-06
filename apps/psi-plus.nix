# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ fetchFromGitHub, psi-plus, libsForQt5 }:

(psi-plus.override { enablePsiMedia = true; }).overrideAttrs (srcAttrs: rec {
  version = "1.5.1609.1";

  src = fetchFromGitHub {
    owner = "psi-plus";
    repo = "psi-plus-snapshots";
    rev = version;
    sha256 = "16q9w345lyrgq0nxn1pxms9zvswxg6av10cb8630gzm56sf4ni4f";
  };

  buildInputs = srcAttrs.buildInputs ++ [
    libsForQt5.qtimageformats
  ];
})
