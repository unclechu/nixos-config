# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{ pkgs ? import <nixpkgs> {}
, lib ? pkgs.lib
, callPackage ? pkgs.callPackage
, symlinkJoin ? pkgs.symlinkJoin
, makeBinaryWrapper ? pkgs.makeBinaryWrapper

, haskell-language-server ? pkgs.haskell-language-server
, haskellPackages ? pkgs.haskellPackages
, haskell ? pkgs.haskell

, libX11 ? pkgs.libX11
, libXtst ? pkgs.libXtst

, dzen2 ? pkgs.dzen2

, executable-dependencies ? callPackage ../../../utils/executable-dependencies.nix {}

# nix-shell arguments
, inNixShell ? false
, withHoogle ? true

# Build options
, __src ? ./.
}:

let
  name = "wenzels-i3-status-generator";

  src =
    let
      extensionOf = path:
        let match = builtins.match ".*\\.([^./]+)$" (builtins.baseNameOf path);
        in if match == null then null else builtins.elemAt match 0;
    in
      builtins.path {
        name = "${name}-source";
        path = __src;

        # Note that `cabal.project` does not end up here (it’s not neccesary for
        # the build). `cabal.project` disables optimizations for HLS and
        # development `cabal build`/`cabal run`/`cabal repl`. But since it’s not
        # added here for the Nix build of the executable default optimization
        # level is applied. So `cabal.project` is intentionally avoided here.
        filter = path: type:
          (
            type == "directory" &&
            builtins.match "^(app|src|[A-Z][A-Za-z0-9]+)$" (baseNameOf path) != null
          ) || (
            type == "regular" &&
            builtins.elem (extensionOf path) ["hs" "hsc" "cabal"]
          );
      };

  e = executable-dependencies {
    dzen2 = dzen2;
  };

  hsPkgs = haskellPackages.extend (self: super: {
    ${name} =
      lib.pipe (self.callCabal2nix name src {}) [
        (lib.flip haskell.lib.addBuildTools [ self.hlint ])
        # Lint the sources before building
        (lib.flip haskell.lib.overrideCabal (old: {
          preConfigure = (old.preConfigure or "") + ''
            hlint -- src/
          '';
        }))
        # Enable level-2 optimizations
        (lib.flip haskell.lib.overrideCabal (old: {
          configureFlags = (old.configureFlags or []) ++ [
            "--enable-optimization=2"
          ];
        }))
      ];
  });

  wenzels-i3-status-generator =
    let pkg = haskell.lib.justStaticExecutables hsPkgs.${name}; in
    symlinkJoin (lib.fix (x: {
      pname = "${name}-wrapped";
      name = x.pname;
      meta.mainProgram = pkg.meta.mainProgram;
      nativeBuildInputs = [ makeBinaryWrapper ];
      paths = [ pkg ];
      postBuild = ''
        wrapProgram "$out"/bin/${lib.escapeShellArg pkg.meta.mainProgram} \
          --prefix PATH : ${lib.escapeShellArg (
            lib.makeBinPath (builtins.attrValues e.executables)
          )}
      '';
    }));

  # LSP for Haskell
  hls = haskell-language-server.override {
    haskellPackages = hsPkgs;

    supportedGhcVersions = [
      (builtins.replaceStrings ["."] [""] hsPkgs.ghc.version)
    ];
  };

  shell = hsPkgs.shellFor {
    packages = p: [
      p.${name}
    ];

    inherit withHoogle;

    shellHook = ''
      # Needed for `cabal repl` and HLS (Haskell Language Server LSP) to work.
      export LD_LIBRARY_PATH=${
        lib.escapeShellArg (lib.makeLibraryPath [libX11 libXtst])
      }"''${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH"
    '';

    buildInputs = [
      hsPkgs.cabal-install
      hsPkgs.hlint
      hls
    ];
  };
in

(if inNixShell then shell else wenzels-i3-status-generator) // {
  inherit shell wenzels-i3-status-generator;
  haskell-language-server = hls;
  haskellPackages = hsPkgs;
  haskellPackage = hsPkgs.${name};
}
