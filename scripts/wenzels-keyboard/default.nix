# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let
  constants = import ../../constants.nix;
in

{ pkgs ? import <nixpkgs> {}

, lib ? pkgs.lib
, callPackage ? pkgs.callPackage
, mkShell ? pkgs.mkShell
, symlinkJoin ? pkgs.symlinkJoin
, makeBinaryWrapper ? pkgs.makeBinaryWrapper
, fetchFromGitHub ? pkgs.fetchFromGitHub
, stdenv ? pkgs.stdenv

, nim ? pkgs.nim
, nim-2_0 ? pkgs.nim-2_0 # npmlsp is not okay to work with newer Nim
, buildNimPackage ? pkgs.buildNimPackage

, nimlsp ? pkgs.nimlsp # Nim LSP
, nimlangserver ? pkgs.nimlangserver # Nim LSP

, coreutils ? pkgs.coreutils
, util-linux ? pkgs.util-linux
, procps ? pkgs.procps
, xset ? pkgs.xset
, setxkbmap ? pkgs.setxkbmap
, numlockx ? pkgs.numlockx
, libnotify ? pkgs.libnotify

, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}
, __xbindkeys ? callPackage ../../apps/wenzels-xbindkeys.nix {}
, __wenzels-xlib-keys-hack ? callPackage ../../apps/wenzels-xlib-keys-hack {}

# nix-shell arguments
, inNixShell ? false

# Build options
, __srcFile ? ./wenzels_keyboard.nim
, __nimLsp ? "nimlsp" # one of: `[null "nimlsp" "nimlangserver"]`
, keyRepeatDelay ? constants.keyRepeat.delay
, keyRepeatInterval ? constants.keyRepeat.interval
, xkbLayout ? constants.xkb.layout
, xkbOptions ? constants.xkb.options
}:

assert builtins.isInt keyRepeatDelay && keyRepeatDelay >= 1;
assert builtins.isInt keyRepeatInterval && keyRepeatInterval >= 1;
assert builtins.isString xkbLayout && xkbLayout != "";
assert builtins.isString xkbOptions && xkbOptions != "";

let
  pkgs = null; # Prevent from using directly

  # TODO: Remove when https://github.com/NixOS/nixpkgs/pull/537634 is released to nixos-26.05
  newer-nimlsp =
    assert lib.getVersion nimlsp == "0.4.6";
    nimlsp.overrideAttrs (old: rec {
      version = "0.4.7";

      src = fetchFromGitHub {
        owner = "PMunch";
        repo = "nimlsp";
        rev = "v${version}";
        hash = "sha256-jUNW+tukZXv41HTWP2F2BdEn7nesFXVg2TffKPWfSss=";
      };
    });

  # Runtime dependencies for the app
  e = (executable-dependencies {
    env = coreutils;
    setsid = util-linux;
    pgrep = procps;
    xset = xset;
    setxkbmap = setxkbmap;
    numlockx = numlockx;
    notify-send = libnotify;
    xbindkeys = __xbindkeys;
    wenzels-xlib-keys-hack = __wenzels-xlib-keys-hack;
  }).extend (final: prev: {
    scriptDependencies = scriptSrc:
      final.dependencies
        ''^[[:space:]]*# Guard dependencies( .*)?$''
        ''^[[:space:]]*needExe[(]"([^"]+)"[)]([[:space:]]*[#].*)?$''
        scriptSrc;
  });

  wenzels-keyboard = stdenv.mkDerivation rec {
    pname = "wenzels-keyboard";
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
        nim check --colors:off --styleCheck:error --hintAsError:XDeclaredButNotUsed:on "$file"
      done
    '';

    buildPhase = ''
      runHook preBuild
      (
        set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
        nim compile -d:release --nimcache:nimcache --mm:atomicArc \
          -o:${lib.escapeShellArg meta.mainProgram} "$src"
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

  wenzels-keyboard-wrapped = symlinkJoin rec {
    pname = "${lib.getName wenzels-keyboard}-wrapped";
    name = pname;
    meta.mainProgram = wenzels-keyboard.meta.mainProgram;
    nativeBuildInputs = [ makeBinaryWrapper ];
    paths = [ wenzels-keyboard ];
    postBuild = ''
      CMD=(
        wrapProgram
        "$out"/bin/${lib.escapeShellArg wenzels-keyboard.meta.mainProgram}
        --prefix PATH : ${lib.escapeShellArg (e.scriptDependenciesBinPath __srcFile)}
        --add-flag --key-repeated-delay=${lib.escapeShellArg keyRepeatDelay}
        --add-flag --key-repeated-interval=${lib.escapeShellArg keyRepeatInterval}
        --add-flag --xkb-layout=${lib.escapeShellArg xkbLayout}
        --add-flag --xkb-options=${lib.escapeShellArg xkbOptions}
      )
      "''${CMD[@]}"
    '';
  };

  shell = mkShell {
    buildInputs =
      (
        builtins.filter
          (x: __nimLsp != "nimlsp" || x != nim)
          wenzels-keyboard.nativeBuildInputs
      )
      ++ wenzels-keyboard.buildInputs
      ++ builtins.attrValues e.executables
      ++ (
        if isNull __nimLsp then [] else
        if __nimLsp == "nimlsp" then [ nim-2_0 newer-nimlsp ] else
        if __nimLsp == "nimlangserver" then [ nimlangserver ] else
        throw "Unexpected __nimLsp: ${builtins.toJSON __nimLsp}"
      )
      ;
  };
in

(if inNixShell then shell else wenzels-keyboard-wrapped) // {
  wenzels-keyboard = wenzels-keyboard-wrapped;
  wenzels-keyboard-unwrapped = wenzels-keyboard;
  inherit shell;
}
