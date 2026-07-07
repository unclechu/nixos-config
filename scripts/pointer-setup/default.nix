# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../../nix/sources.nix; in

{ pkgs ? import sources.nixpkgs-master {}

, lib ? pkgs.lib
, callPackage ? pkgs.callPackage

, jq ? pkgs.jq

, xinput ? pkgs.xinput
, libnotify ? pkgs.libnotify

, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}
, mk-nim-app ? callPackage ../../utils/nim/mk-nim-app.nix {}
, __xbindkeys ? callPackage ../../apps/wenzels-xbindkeys.nix {}
, __wenzels-xlib-keys-hack ? callPackage ../../apps/wenzels-xlib-keys-hack {}
, __clunky-toml-json-converter ? callPackage ../../apps/clunky-toml-json-converter {}

# nix-shell arguments
, inNixShell ? false
, __nimLsp ? "nimlsp" # one of: `[null "nimlsp" "nimlangserver"]`

# Build options
, __srcFile ? ./pointer_setup.nim
}:

let
  pkgs = null; # Prevent from using directly

  e = executable-dependencies {
    xinput = xinput;
    notify-send = libnotify;
    jq = jq;
    clunky-toml-json-converter = __clunky-toml-json-converter;
  };

  extraSrcFiles = [
    ./nim.cfg
    ./needexe.nim
    ../../utils/nim/cliargs.nim
    ../../utils/nim/log.nim
    ../../utils/nim/either.nim
  ];

  name = "pointer-setup";
  src = __srcFile;

  make-pointer-setup-wrapper =
    args@
    { pointer-name ? null
    , device-name ? null
    , device-product-id ? null
    , set-props ? {}
    }:
    assert pointer-name != null -> builtins.isString pointer-name;
    assert pointer-name != null -> builtins.match ".*[[:space:]].*" pointer-name == null;
    assert device-name != null -> builtins.isString device-name;
    assert device-product-id != null -> builtins.isString device-product-id;
    assert builtins.isAttrs set-props;
    assert builtins.all (values:
      builtins.isList values && builtins.all builtins.isString values
    ) (builtins.attrValues set-props);
    mk-nim-app {
      inherit name src extraSrcFiles e;
      wrapProgramArgs =
        lib.pipe args [
          (lib.filterAttrs (n: v: builtins.elem n [
            "pointer-name"
            "device-name"
            "device-product-id"
          ]))
          (lib.foldlAttrs (acc: n: v:
            if isNull v then acc else acc ++ ["--add-flag" "--${n}=${v}"]
          ) [])
        ]
        ++ lib.pipe set-props [
          (lib.foldlAttrs (acc: n: v: acc ++ ["--add-flag" "--set-prop=${n}=${
            builtins.concatStringsSep " " v
          }"]) [])
        ]
        ;
      renameExecutableTo = if isNull pointer-name then null else pointer-name;
      lspForShell = __nimLsp;
    };

  pointer-setup = make-pointer-setup-wrapper {};
in

(if inNixShell then pointer-setup.shell else pointer-setup) // {
  inherit pointer-setup make-pointer-setup-wrapper;
  inherit (pointer-setup) shell;
}
