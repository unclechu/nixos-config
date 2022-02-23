# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ fetchFromGitHub, psi-plus, libsForQt5 }:

(psi-plus.override { enablePsiMedia = true; }).overrideAttrs (srcAttrs: rec {
  version = "1.5.1605";

  src = fetchFromGitHub {
    owner = "psi-plus";
    repo = "psi-plus-snapshots";
    rev = version;
    sha256 = "1x7pysjjhvchqs5y5apm9wfvm95zczhqvgi67zai69a7glvw3bjj";
  };

  buildInputs = srcAttrs.buildInputs ++ [
    libsForQt5.qtimageformats
  ];
})
