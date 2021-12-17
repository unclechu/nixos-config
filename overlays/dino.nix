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
      rev = "be0233000cd69fffd1257017d7792a0cf404434f"; # 14 December 2021
      sha256 = "08pr4hsgak5p15k3cz3alngd5ja3319d43z3ifgliq1i9hkjqmdm";
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
