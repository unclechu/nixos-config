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

, pkg-config ? pkgs.pkg-config
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
        (lib.flip haskell.lib.overrideCabal (old: {
          # Lint the sources before building
          preConfigure = (old.preConfigure or "") + ''
            # TODO: Refactor the code following the linter warnings and enable this
            # hlint -- src/
          '';
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

    buildInputs = [
      hsPkgs.cabal-install
      hsPkgs.hlint
      hls
    ];
  };
in

(if inNixShell then shell else wenzels-i3-status-generator) // {
  inherit shell;
  haskell-language-server = hls;
  haskellPackages = hsPkgs;
  haskellPackage = hsPkgs.${name};
}
