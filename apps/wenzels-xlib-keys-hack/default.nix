# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../../nix/sources.nix; in
{ callPackage

, coreutils
, util-linux
, dash
, xinput
, gnused
, findutils

# Overridable dependencies
, __xlib-keys-hack ? callPackage sources.xlib-keys-hack {}
, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../../utils/mk-generic-script.nix {}

# Build options
, __srcScript ? ./main.bash
}:

let
  e = executable-dependencies {
    xlib-keys-hack = __xlib-keys-hack;
    sleep = coreutils;
    setsid = util-linux;
    dash = dash;
    xinput = xinput;
    sed = gnused;
    xargs = findutils;
    sort = coreutils;
  };
in

mk-generic-script rec {
  name = "wenzels-xlib-keys-hack";
  src = __srcScript;
  inherit e;
  dontAddDependencies = true;
  cutOffRuntimeDependenciesCheckPhase = null;

  wrapProgramArgs = [
    "--prefix" "PATH" ":" (e.scriptDependenciesBinPathWithIgnore src [
      # It must have root sticky bit, thus coming from system dependencies.
      "grant-access-to-input-devices"
    ])
  ];
}
