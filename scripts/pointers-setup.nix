# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ lib
, callPackage
, writeText
, dash
, xinput

# Overridable dependencies
, __pointers ? callPackage ./pointers.nix {}
, executable-dependencies ? callPackage ../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../utils/mk-generic-script.nix {}
}:
let
  name = "pointers-setup";

  pointers = lib.filterAttrs (n: v: lib.isDerivation v) __pointers;

  e = executable-dependencies ({
    dash = dash;
    xinput = xinput;
  } // pointers);

  src = writeText "${name}-source" ''
    #! /usr/bin/env dash
    exec <&- 1>/dev/null 2>/dev/null # silent

    # all pointer setup scripts (mice)
    ${lib.pipe pointers [
      builtins.attrNames
      (map (name: "${e.s.${name}} &"))
      (builtins.concatStringsSep "\n")
    ]}

    # laptop touchscreen
    ${e.s.xinput} --map-to-output 'ELAN25B6:00 04F3:0732' eDP1

    # prevent returning exit status of the latest command (it’s okay to fail)
    exit 0
  '';
in

mk-generic-script {
  inherit name src e;
  dontAddDependencies = true;
  buildInputs = [ e.executables.dash ];
}
