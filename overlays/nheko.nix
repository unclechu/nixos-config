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

  mtxclient = super.mtxclient.overrideAttrs (srcAttrs: srcAttrs // rec {
    version = "git";

    # Took this pin from here:
    # https://github.com/Nheko-Reborn/nheko/blob/d84c1f59/io.github.NhekoReborn.Nheko.yaml#L164-L168
    src = super.fetchFromGitHub {
      owner = "Nheko-Reborn";
      repo = "mtxclient";
      rev = "a368db306d0148d1c8c17a532ebe4ff05a2a08c8"; # 17 August 2021
      sha256 = "1rxx4aig5fajd6paqr5nwbprwc98cidmzrsfww48w7vmff14z0w4";
    };

    cmakeFlags = srcAttrs.cmakeFlags ++ [
      "-DCMAKE_CXX_FLAGS=-DSPDLOG_FMT_EXTERNAL" # HACK: Overwrites all other CXX_FLAGS!
    ];

    buildInputs = builtins.filter (
      x: let n = x.pname or ""; in n != "coeurl"
    ) srcAttrs.buildInputs ++ [
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
      rev = "d97e408c98b05745265202e4e7f45aed9542930c"; # 25 August 2021
      sha256 = "0rimmdgrs64xhk4cbrps02wxydjai7w501rgkqwcr1kqcxal1cml";
    };

    buildInputs = builtins.filter (
      x: let n = x.pname or ""; in n != "coeurl" && n != "mtxclient"
    ) srcAttrs.buildInputs ++ [
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
