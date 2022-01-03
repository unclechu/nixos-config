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
      rev = "6b8ad7a1044f3f01cc0789e912800350a64c0c2c"; # 1 January 2022
      sha256 = "03zyk5m8cqc2a8n669b1pfp37fimpy7aghxhghkbr4b6jdd6xjn5";
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
