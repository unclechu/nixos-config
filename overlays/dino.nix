# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
self: super:
{
  # It releases rarely and some features are provided in “master” branch only
  dino = super.dino.overrideAttrs (srcAttrs: srcAttrs // rec {
    version = "git-master-after-v0.2.2";

    src = super.fetchFromGitHub {
      owner = "dino";
      repo = "dino";
      rev = "1e63cb3bd935ad42af7b203efc5f4f83ae131e58"; # 29 January 2022
      sha256 = "0gjkpd7qyq93hv60js8y0dj1kdngz7dsa49kx256v20wycms3dca";
    };

    buildInputs = srcAttrs.buildInputs ++ [
      super.utillinux
      super.gst_all_1.gst-plugins-base
      super.gst_all_1.gst-plugins-good
      super.libselinux
      super.gspell
      super.libunwind
      super.libsepol
      super.srtp
      super.sysprof
      super.libnice
      super.gnutls
      super.libthai
      super.libpsl
      super.libdatrie
      super.elfutils
      super.libtasn1
      super.brotli
      super.xorg.libXtst
      super.p11-kit
      super.orc
      super.gupnp-igd
    ];
  });
}
