# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
self: super:
{
  xorg = super.xorg // {
    xf86videoamdgpu = super.callPackage ({
      stdenv, pkg-config, fetchurl, xorgproto, mesa, libGL, libdrm, udev, xorgserver
    }: stdenv.mkDerivation {
      pname = "xf86-video-amdgpu";
      version = "21.0.0";
      builder = <nixpkgs> + "/pkgs/servers/x11/xorg/builder.sh";
      src = fetchurl {
        url = "mirror://xorg/individual/driver/xf86-video-amdgpu-21.0.0.tar.bz2";
        sha256 = "125dq85n46yqmnmr2hknxwcqicwlvz2b2phf0m963fpg9l1j6y30";
      };
      hardeningDisable = [ "bindnow" "relro" ];
      nativeBuildInputs = [ pkg-config ];
      buildInputs = [ xorgproto mesa libGL libdrm udev xorgserver ];
      meta.platforms = super.lib.platforms.unix;
    }) {};
  };
}
