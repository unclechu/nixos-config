# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ lib
, callPackage
, writeText

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
    xinput = xinput;
  } // pointers);

  src = writeText "${name}-source" ''
    #! /usr/bin/env bash
    set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
    exec <&-

    pids=()

    # All pointer setup scripts (mice)
    ${lib.pipe pointers [
      builtins.attrNames
      (map (x: ''(set -o xtrace; ${e.s.${x}} 2>/dev/null) & pids+=("$!")''))
      (builtins.concatStringsSep "\n")
    ]}

    # Assign laptop touchscreen to the laptop display (by default it expands to
    # all screens and when you touch it it sends the cursor to who knows where)
    (
      set -o xtrace
      ${e.s.xinput} --map-to-output 'ELAN25B6:00 04F3:0732' eDP1 2>/dev/null
    ) & pids+=("$!")

    # Wait for all background jobs to finish.
    # Wait for each individually to avoid `wait` exiting early on failure
    # of any of the jobs.
    for pid in "''${pids[@]}"; do wait -- "$pid" || :; done

    # Prevent returning exit status of the latest command (it’s okay to fail)
    exit 0
  '';
in

mk-generic-script {
  inherit name src e;
  dontAddDependencies = true;
}
