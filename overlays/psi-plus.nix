# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
self: super:
{
  psi-plus = super.psi-plus.overrideAttrs (srcAttrs: srcAttrs // rec {
    version = "1.4.1520"; # Latest on October 18, 2020

    src = super.fetchFromGitHub {
      owner = "psi-plus";
      repo = "psi-plus-snapshots";
      rev = version;
      sha256 = "17ih8c1k4wabrp172p4hsywil8ad5c53r4jh9zc1p7f499083srl";
    };

    cmakeFlags = [
      "-DENABLE_PLUGINS=ON"
      "-DCHAT_TYPE=BASIC"
    ];
  });
}
