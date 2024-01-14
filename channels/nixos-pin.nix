# A “nixpkgs” native Nix pin made of “nixos” channel pin of this NixOS configuration.
#
# It does not force you to fetch the tarball first via “manage.raku” script.
# Instead it extracts and parses the pin data and makes a native “fetchTarball”.
# This is useful for running some scripts of this repo (such as “manage.raku”)
# in a NixOS Live CD environment (when applying this NixOS configuration on a
# new piece of hardware) using determenistic “nixpkgs” pin where system
# <nixpkgs> channel is not overridden yet.
#
# Usage example (from the project root):
#
#   nix eval --impure --expr '(import (import channels/nixos-pin.nix) {}).lib.version'
#
# Also look at the “manage.raku”’s first few shebang lines to see how it can be
# used for the standalone scripts.

let
  release-link = builtins.readFile nixos/release-link;
  sha256 = builtins.readFile nixos/nixexprs-unpacked-sha256-checksum;
in

fetchTarball {
  url = "${release-link}/nixexprs.tar.xz";
  inherit sha256;
}
