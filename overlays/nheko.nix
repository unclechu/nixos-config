# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
self: super:
{
  # Copy-pasted with some modifications from here:
  # https://github.com/NixOS/nixpkgs/pull/129715
  coeurl = super.stdenv.mkDerivation {
    pname = "coeurl";
    version = "git";

    src = super.fetchFromGitLab {
      domain = "nheko.im";
      owner = "nheko-reborn";
      repo = "coeurl";
      rev = "e9010d1ce14e7163d1cb5407ed27b23303781796"; # 7 July 2021
      sha256 = "1as81hmfjhnk1ghqwv815hg9wi0x177v4sghkrmczrd34sfxjhq4";
    };

    nativeBuildInputs = [
      super.meson
      super.ninja
      super.pkg-config
    ];

    buildInputs = [
      super.curl
      super.libevent
      super.spdlog
    ];

    doCheck = true;

    meta = {
      description = "A simple async wrapper around CURL for C++";
      homepage = "https://nheko.im/nheko-reborn/coeurl";
      license = super.lib.licenses.mit;
      platforms = super.lib.platforms.all;
    };
  };

  mtxclient = super.mtxclient.overrideAttrs (srcAttrs: srcAttrs // rec {
    version = "0.5.1-git";

    # Took this pin from here:
    # https://github.com/Nheko-Reborn/nheko/blob/d84c1f59/io.github.NhekoReborn.Nheko.yaml#L164-L168
    src = super.fetchFromGitHub {
      owner = "Nheko-Reborn";
      repo = "mtxclient";
      rev = "bcf363cb5e6c423f40c96123e227bc8c5f6d6f80"; # 7 August 2021
      sha256 = "1zwlnacixnvg7mb45lmg30ql4k98rg9jd0zzmld64r9hac2qyd1m";
    };

    cmakeFlags = srcAttrs.cmakeFlags ++ [
      "-DCMAKE_CXX_FLAGS=-DSPDLOG_FMT_EXTERNAL" # HACK: Overwrites all other CXX_FLAGS!
    ];

    buildInputs = srcAttrs.buildInputs ++ [
      self.coeurl
      super.curl
      super.libevent
    ];
  });

  # Get a freshier version of nheko
  nheko = super.nheko.overrideAttrs (srcAttrs: srcAttrs // {
    version = "git-master";

    src = super.fetchFromGitHub {
      owner = "Nheko-Reborn";
      repo = "nheko";
      rev = "d84c1f59a45582ea97f4fd8f969c5e9917b3af2c"; # 11 August 2021
      sha256 = "0v4pkcvjf6ydskbvj1j6bnk4hphwnhr4rn0hfcb3insfg83slwg4";
    };

    buildInputs = builtins.filter (x: x.pname or "" != "mtxclient") srcAttrs.buildInputs ++ [
      self.coeurl
      self.mtxclient
      super.curl
      super.elfutils
      super.libevent
      super.libunwind
      super.pcre
      super.xorg.libXdmcp
    ];

    postPatch = ''
      ${srcAttrs.postPatch or ""}
      substituteInPlace CMakeLists.txt --replace \
        "# Fixup bundled keychain include dirs" \
        "find_package(Boost COMPONENTS iostreams system  thread REQUIRED)"
    '';

    cmakeFlags = srcAttrs.cmakeFlags ++ [
      "-DCOMPILE_QML=ON" # see https://github.com/Nheko-Reborn/nheko/issues/389
      "-DBUILD_SHARED_LIBS=OFF"
    ];
  });
}
