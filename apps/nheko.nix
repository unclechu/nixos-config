# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let
  coeurl-overlay = self: super: {
    # Copy-pasted with some modifications from here:
    # https://github.com/NixOS/nixpkgs/pull/129715
    coeurl = super.stdenv.mkDerivation {
      pname = "coeurl";
      version = "git";

      src = super.fetchFromGitLab {
        domain = "nheko.im";
        owner = "nheko-reborn";
        repo = "coeurl";
        rev = "3901507db25cf3f9364b58cd8c7880640900c992"; # 12 August 2021
        sha256 = "1sz7mifc6nk80xg6jp8wbkdmdv1kq0igx0v3xkn722mw50fhxqby";
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
      version = "git";

      # Took this pin from here:
      # https://github.com/Nheko-Reborn/nheko/blob/54723b06/io.github.NhekoReborn.Nheko.yaml#L164-L168
      src = super.fetchFromGitHub {
        owner = "Nheko-Reborn";
        repo = "mtxclient";
        rev = "e5284ccc9d902117bbe782b0be76fa272b7f0a90"; # 13 October 2021
        sha256 = "05syiyc5xkvqisia4f2kk0r7h8qgig04m4y0g027wbyx1fhd5qis";
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

  pkgs = import <nixos-unstable> {
    overlays = [
      coeurl-overlay
      mtxclient-overlay
    ];
  };
in
# Get a freshier version of nheko
pkgs.nheko.overrideAttrs (srcAttrs: srcAttrs // {
  version = "git-master";

  src = pkgs.fetchFromGitHub {
    owner = "Nheko-Reborn";
    repo = "nheko";
    rev = "417cc07172141763ac8b79143457116d9a423a0a"; # 1 November 2021
    sha256 = "1dnhv541vqd9hag73sfx3qgr9lwnbi5bq0np67q7ddsxs1bcyc9m";
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
