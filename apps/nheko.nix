# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# See this file for pins of the dependencies for Nheko (mtxclient & coeurl):
# https://github.com/Nheko-Reborn/nheko/blob/master/io.github.NhekoReborn.Nheko.yaml

args@{ lib, ... }:

let
  coeurl-overlay = self: super: {
    # Copy-pasted with some modifications from here:
    # https://github.com/NixOS/nixpkgs/pull/129715
    coeurl = super.stdenv.mkDerivation rec {
      pname = "coeurl";
      version = "v0.1.0";

      src = super.fetchFromGitLab {
        domain = "nheko.im";
        owner = "nheko-reborn";
        repo = "coeurl";
        rev = version; # 14 November 2021
        sha256 = "10a5klr44m2xy6law8s3s5rynk1q268fa4pkhilbn52yyv0fwajq";
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
  };

  mtxclient-overlay = self: super: {
    mtxclient = super.mtxclient.overrideAttrs (srcAttrs: srcAttrs // rec {
      version = "v0.6.0";

      patches = []; # Remove the outdated patch that fails to be applied

      src = super.fetchFromGitHub {
        owner = "Nheko-Reborn";
        repo = "mtxclient";
        rev = version; # 17 November 2021
        sha256 = "0sxx7vj6a1n2d95c118pjq52707qwf16154fdvz5f4z1pq7c8dsi";
      };

      cmakeFlags = srcAttrs.cmakeFlags ++ [
        "-DCMAKE_CXX_FLAGS=-DSPDLOG_FMT_EXTERNAL" # HACK: Overwrites all other CXX_FLAGS!
      ];

      buildInputs = srcAttrs.buildInputs ++ [
        super.coeurl
        super.curl
        super.libevent
      ];
    });
  };

  pkgs = lib.pipe args.pkgs [
    (x: x.extend coeurl-overlay)
    (x: x.extend mtxclient-overlay)
  ];
in
# Get a freshier version of nheko
(pkgs.nheko.override {
  # At least 0.12.0 is mandatory for nheko to work.
  # At the moment of writing this in NixOS 21.05 it was 0.9.1, nheko fails at startup.
  # I’ve taken this newer pin from nixos-unstable.
  qtkeychain = pkgs.libsForQt5.qtkeychain.overrideAttrs (srcAttrs: rec {
    name = "qtkeychain-${version}";
    version = "0.12.0";

    src = pkgs.fetchFromGitHub {
      owner = "frankosterfeld";
      repo = "qtkeychain";
      rev = "v${version}";
      sha256 = "0gi1nx4bcc1vwfw41cif3xi2i59229vy0kc2r5959d8n6yv31kfr";
    };

    # Remove patches. In nixos-unstable there is a patch for Darwin but I don’t use Darwin so I
    # don’t care about it at the moment.
    patches = [];
  });
}).overrideAttrs (srcAttrs: srcAttrs // rec {
  version = "v0.9.0";

  src = pkgs.fetchFromGitHub {
    owner = "Nheko-Reborn";
    repo = "nheko";
    rev = version; # 19 November 2021
    sha256 = "1akhnngxkxbjwjkg5ispl6j5s2ylbcj92r3zxqqry4gbfxbjpx8k";
  };

  buildInputs = srcAttrs.buildInputs ++ [
    pkgs.coeurl
    pkgs.mtxclient
    pkgs.curl
    pkgs.elfutils
    pkgs.libevent
    pkgs.libunwind
    pkgs.pcre
    pkgs.xorg.libXdmcp
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
})
