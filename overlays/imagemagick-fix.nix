# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# Fix build for Shutter.
self: super:
let
  imagemagick = super.imagemagick.overrideAttrs (_: {
    # Picked from https://github.com/NixOS/nixpkgs/pull/156920
    # It’s already merged to “release-21.11”, so it should be soon available in one of the next
    # releases of “nixos-21.11” channel. TODO: Remove this overlay when it’s not needed anymore.
    patches = [
      # fix a type confusion bug introduced in 7.1.0-20 with commit 075565e93c71bcaaabf0ce70b7d1060bccdf0020
      (super.fetchpatch {
        url = "https://github.com/ImageMagick/ImageMagick/commit/62845d5672eca4446b952dd0ab2e3e0dab0309d4.patch";
        sha256 = "1kni5i8b5hl69niypidm90mhir8cafi6r9i857fxdlv045h3dg4p";
      })
    ];
  });

  perlPackages = super.perlPackages.override {
    pkgs = super // { inherit imagemagick; };
  };
in
{
  shutter = super.shutter.override {
    inherit imagemagick perlPackages;
  };
}
