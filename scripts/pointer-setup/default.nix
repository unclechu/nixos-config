# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let
  constants = import ../../constants.nix;
  sources = import ../../nix/sources.nix;
in

{ pkgs ? import sources.nixpkgs-master {}

, lib ? pkgs.lib
, callPackage ? pkgs.callPackage
, mkShell ? pkgs.mkShell
, symlinkJoin ? pkgs.symlinkJoin
, makeBinaryWrapper ? pkgs.makeBinaryWrapper
, fetchFromGitHub ? pkgs.fetchFromGitHub
, stdenv ? pkgs.stdenv

, nim ? pkgs.nim
, buildNimPackage ? pkgs.buildNimPackage

# LSP
, nimlsp ? pkgs.nimlsp # Nim LSP
, nimlangserver ? pkgs.nimlangserver # Nim LSP

, jq ? pkgs.jq

, xinput ? pkgs.xinput
, libnotify ? pkgs.libnotify

, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}
, __xbindkeys ? callPackage ../../apps/wenzels-xbindkeys.nix {}
, __wenzels-xlib-keys-hack ? callPackage ../../apps/wenzels-xlib-keys-hack {}
, __clunky-toml-json-converter ? callPackage ../../apps/clunky-toml-json-converter {}

# nix-shell arguments
, inNixShell ? false
, __nimLsp ? "nimlsp" # one of: `[null "nimlsp" "nimlangserver"]`

# Build options
, __config ? ./config.toml
, __srcFile ? ./pointer_setup.nim
, __nimModulesSrc ?
    [
      ../../utils/nim/cliargs.nim
      ../../utils/nim/log.nim
      ../../utils/nim/either.nim
    ]
}:

let
  pkgs = null; # Prevent from using directly
  config = builtins.fromTOML (builtins.readFile __config);

  # Runtime dependencies for the app
  e = (executable-dependencies {
    xinput = xinput;
    notify-send = libnotify;
    jq = jq;
    clunky-toml-json-converter = __clunky-toml-json-converter;
  }).extend (final: prev: {
    scriptDependencies = scriptSrc:
      final.dependencies
        ''^[[:space:]]*# Guard dependencies( .*)?$''
        ''^[[:space:]]*needExe[(]"([^"]+)"[)]([[:space:]]*[#].*)?$''
        scriptSrc;
  });

  pointer-setup = stdenv.mkDerivation rec {
    pname = "pointer-setup";
    name = pname;
    meta.mainProgram = name;
    src = __srcFile;
    inherit (e) checkPhase;

    dontUnpack = true;
    doCheck = true;

    nativeBuildInputs = [
      nim
    ];

    prePatch = ''
      # Add dependencies
      ${lib.pipe __nimModulesSrc [
        (map (x: ''cp -- ${lib.escapeShellArg "${x}"} ${lib.escapeShellArg (baseNameOf x)}''))
        (builtins.concatStringsSep "\n")
      ]}

      # Nim is not happy about dashes in the name but Nix adds some hashes prefix with a dash after.
      # Getting rid of them by `''${foo##*-}`.

      pre_patched_src=''${src##*/}
      pre_patched_src=original_''${pre_patched_src##*-}
      cp -- "$src" "$pre_patched_src"

      _new_src=''${src##*/}
      _new_src=''${_new_src##*-}

      cp -- "$src" "$_new_src"
      src="$_new_src"

      (
        set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail

        # The dependencies are already checked, no need to do it in runtime.
        sed -i '/^\s*# Guard dependencies\( .*\)\?$/,/^$/d' "$src"

        # Make sure the replacement was successful
        a=$(<"$pre_patched_src"); b=$(<"$src")
        if [[ "$a" == "$b" ]]; then
          >&2 echo 'Failed to remove runtime dependencies checking'
          exit 1
        fi
      )
    '';

    preConfigure = ''
      for file in "$pre_patched_src" "$src"; do
        nim check ${lib.escapeShellArgs config.nim.lint-arguments} "$file"
      done
    '';

    buildPhase = ''
      runHook preBuild
      (
        set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
        nim compile ${
          lib.escapeShellArgs config.nim.nix-build-extra-arguments
        } ${
          lib.escapeShellArgs config.nim.build-arguments
        }
      )
      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      (
        set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
        mkdir -p -- "$out"/bin
        cp -- ${lib.escapeShellArg meta.mainProgram} "$out"/bin
      )
      runHook postInstall
    '';
  };

  make-pointer-setup-wrapper =
    args@
    { pointer-name ? null
    , device-name ? null
    , device-product-id ? null
    , set-props ? {}
    , pname ? if isNull pointer-name then "${lib.getName pointer-setup}-wrapped" else pointer-name
    }:
      assert pointer-name != null -> builtins.isString pointer-name;
      assert pointer-name != null -> builtins.match ".*[[:space:]].*" pointer-name == null;
      assert device-name != null -> builtins.isString device-name;
      assert device-product-id != null -> builtins.isString device-product-id;
      assert builtins.match ".*[[:space:]].*" pname == null;
      assert builtins.isAttrs set-props;
      assert builtins.all (values:
        builtins.isList values && builtins.all builtins.isString values
      ) (builtins.attrValues set-props);
      symlinkJoin {
        inherit pname;
        name = pname;
        meta.mainProgram = pointer-setup.meta.mainProgram;
        nativeBuildInputs = [ makeBinaryWrapper ];
        paths = [ pointer-setup ];
        postBuild = ''
          CMD=(
            ${
              if isNull pointer-name then ''
                wrapProgram
                "$out"/bin/${lib.escapeShellArg pointer-setup.meta.mainProgram}
              '' else ''
                makeWrapper
                "$out"/bin/${lib.escapeShellArg pointer-setup.meta.mainProgram}
                "$out"/bin/${lib.escapeShellArg pointer-name}
              ''
            }
            --prefix PATH : ${lib.escapeShellArg (e.scriptDependenciesBinPath __srcFile)}
            ${lib.pipe args [
              (lib.filterAttrs (n: v: builtins.elem n [
                "pointer-name"
                "device-name"
                "device-product-id"
              ]))
              (lib.foldlAttrs (acc: n: v:
                if isNull v then acc else acc ++ ["--add-flag" "--${n}=${v}"]
              ) [])
              lib.escapeShellArgs
            ]}
            ${lib.pipe set-props [
              (lib.foldlAttrs (acc: n: v: acc ++ ["--add-flag" "--set-prop=${n}=${
                builtins.concatStringsSep " " v
              }"]) [])
              lib.escapeShellArgs
            ]}
          )
          "''${CMD[@]}"
        '';
      };

  pointer-setup-wrapped = make-pointer-setup-wrapper {};

  shell = mkShell {
    buildInputs =
      pointer-setup.nativeBuildInputs
      ++ pointer-setup.buildInputs
      ++ builtins.attrValues e.executables
      ++ (
        if isNull __nimLsp then [] else
        if __nimLsp == "nimlsp" then [ nimlsp ] else
        if __nimLsp == "nimlangserver" then [ nimlangserver ] else
        throw "Unexpected __nimLsp: ${builtins.toJSON __nimLsp}"
      )
      ++ [
        e.executables.clunky-toml-json-converter
        e.executables.jq
      ]
      ;
  };
in

(if inNixShell then shell else pointer-setup-wrapped) // {
  pointer-setup = pointer-setup-wrapped;
  pointer-setup-unwrapped = pointer-setup;
  inherit shell make-pointer-setup-wrapper;
}
