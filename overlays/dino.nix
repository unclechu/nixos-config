# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
self: super:
{
  # It releases rarely and some features are provided in “master” branch only
  dino = super.dino.overrideAttrs (srcAttrs: srcAttrs // rec {
    version = "git-master";

    src = super.fetchFromGitHub {
      owner = "dino";
      repo = "dino";
      rev = "44c0e65129254a02828f0f169da4c01d5cbf8e48"; # 13 October 2021
      sha256 = "1cm9n51p53l0f7rvz0md9q06ryrx3jjwandrjkhm0nqab3pmdm2g";
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
