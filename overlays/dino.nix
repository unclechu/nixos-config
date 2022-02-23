# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
self: super:
{
  # It releases rarely and some features are provided in “master” branch only
  dino = super.dino.overrideAttrs (srcAttrs: srcAttrs // rec {
    version = "0.3.0";

    src = super.fetchFromGitHub {
      owner = "dino";
      repo = "dino";
      rev = "v${version}";
      sha256 = "1m6v3880azv9g5klbzbikpj89wvfvfk4sipqbzw5makxa51bk5ig";
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
