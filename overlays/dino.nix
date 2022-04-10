# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../nix/sources.nix; in

self: super:
{
  dino = super.dino.overrideAttrs (srcAttrs: srcAttrs // rec {
    version = sources.dino.branch;
    src = sources.dino;

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
