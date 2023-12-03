# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ lib
, writeTextFile
, bash
, jack2
, jack-example-tools
}:
let
  executables = {
    bash = bash;
    jack_control = jack2;
    jack_bufsize = jack-example-tools;
  };

  esc = lib.escapeShellArg;
  bin = pkg: exe: "${pkg}/bin/${exe}";
  e = builtins.mapAttrs (n: v: esc (bin v n)) executables;
  executableFileCheck = x: "[[ -f ${x} || -r ${x} || -x ${x} ]]";
in
writeTextFile rec {
  name = "rt-audio";
  executable = true;
  destination = "/bin/${name}";
  checkPhase = ''(
    set -o nounset
    ${builtins.concatStringsSep "\n" (map (x: ''
      if ! ${executableFileCheck x}; then (set -o xtrace && ${executableFileCheck x}); fi
    '') (builtins.attrValues e))}
  )'';
  text = ''
    #! ${let n = "bash"; in bin executables.${n} n}
    set -o errexit || exit

    export PATH=${
      esc (lib.makeBinPath (builtins.attrValues executables))
    }''${PATH:+:}''${PATH}

    ${builtins.readFile ./rt-audio.sh}
  '';
}
