# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ callPackage
, lib
, dash
, libnotify

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}
}:
let
  inherit (__nix-utils) esc writeCheckedExecutable nameOfModuleFile shellCheckers;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  dependencies = {
    dash = dash;
    notify-send = libnotify;
  };

  executables = builtins.mapAttrs (n: v: "${v}/bin/${n}") dependencies;

  checkPhase =
    builtins.concatStringsSep "\n"
      (map shellCheckers.fileIsExecutable (builtins.attrValues executables));
in
writeCheckedExecutable name checkPhase ''
  #! ${executables.dash}
  set -o errexit || exit; set -o nounset; set -o pipefail
  set -o xtrace
  exec ${esc executables.notify-send} -u critical -- "$@"
''
