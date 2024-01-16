# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# Polybar runner script wrapped into a Nix derivation.
#
# It helps to not care about the script location.
# Just rely on the presence of “run-polybar” in the “PATH”.

{ lib
, symlinkJoin
, makeWrapper
, writeTextFile
, writeText
, bash
, polybar # Intended to be provided by “apps/polybar.nix”
, gnugrep
, coreutils
, inotify-tools
}:

let
  esc = lib.escapeShellArg;

  # Mapping between executable file name and a package that provides it.
  dependencies = {
    bash = bash;

    # Keep below dependencies up to date with “Guard dependencies” section in the script.

    polybar = polybar;
    polybar-msg = polybar;
    inotifywait = inotify-tools;
    grep = gnugrep;
    cut = coreutils;
  };

  # Take dependency executable path
  d = e: lib.makeBinPath [dependencies.${e}] + "/" + e;

  # Check one executable dependency
  checkDep = e: ''
    if [[ ! -f ${esc (d e)} || ! -r ${esc (d e)} || ! -x ${esc (d e)} ]]; then (
      set -o xtrace
      [[ -f ${esc (d e)} ]]
      [[ -r ${esc (d e)} ]]
      [[ -x ${esc (d e)} ]]
    ) fi
  '';

  polybar-config-file = writeText "polybar-config-file" (builtins.readFile ./config.ini);

  run-polybar = writeTextFile rec {
    name = "run-polybar";
    destination = "/bin/${name}";
    executable = true;
    text = ''
      #! ${d "bash"}
      POLYBAR_CONFIG_FILE=${esc polybar-config-file}
      ${builtins.readFile ./run-polybar.sh}
    '';
    checkPhase = lib.pipe dependencies [
      builtins.attrNames
      (map checkDep)
      (builtins.concatStringsSep "\n")
    ];
  };

  # A wrapper with attached dependecies
  run-polybar-wrap = symlinkJoin {
    name = "run-polybar-wrap";
    nativeBuildInputs = [ makeWrapper ];
    paths = [ run-polybar ];
    postBuild = ''
      wrapProgram "$out"/bin/${esc (lib.getName run-polybar)} \
        --prefix PATH : ${esc (lib.makeBinPath (builtins.attrValues dependencies))}
    '';
  };
in

run-polybar-wrap
//
{
  inherit
    polybar-config-file
    run-polybar
    ;
}
