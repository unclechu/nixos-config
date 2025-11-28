# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ lib
, writeTextFile
, bash
, coreutils
, gnused
, gnugrep
, xorg
, xdotool
, tmux
, skim
}:
let
  executables = {
    bash = bash;
    sort = coreutils;
    head = coreutils;
    tail = coreutils;
    cut = coreutils;
    sleep = coreutils;
    realpath = coreutils;
    nohup = coreutils;
    dirname = coreutils;
    basename = coreutils;
    sed = gnused;
    grep = gnugrep;
    sk = skim;
    xdotool = xdotool;
    xprop = xorg.xprop;
    tmux = tmux;
  };

  esc = lib.escapeShellArg;
  bin = pkg: exe: "${pkg}/bin/${exe}";
  e = builtins.mapAttrs (n: v: esc (bin v n)) executables;
  executableFileCheck = x: "[[ -f ${x} || -r ${x} || -x ${x} ]]";

  mkScript = action: srcFile: extraEnvMap: alacrittyPkg:
    let alacrittyExe = "${alacrittyPkg}/bin/${lib.getName alacrittyPkg}"; in
    writeTextFile rec {
      name = "tmuxed-${lib.getName alacrittyPkg}-${action}";
      executable = true;
      destination = "/bin/${name}";
      checkPhase = ''(
        set -o nounset
        ${builtins.concatStringsSep "\n" (map (x: ''
          if ! ${executableFileCheck x}; then (set -o xtrace && ${executableFileCheck x}); fi
        '') (builtins.attrValues e ++ [alacrittyExe]))}
      )'';
      text = ''
        #! ${let n = "bash"; in bin executables.${n} n}
        set -o errexit || exit

        export PATH=${
          esc (lib.makeBinPath (builtins.attrValues executables))
        }''${PATH:+:}''${PATH}

        TMUX_EXE=${e.tmux}
        ALACRITTY_EXE=${esc alacrittyExe}
        SKIM_EXE=${e.sk}

        ${lib.pipe extraEnvMap [
          (x: assert builtins.isAttrs x; x)
          lib.attrsToList
          (map (x:
            assert builtins.match "^[A-Z_]+$" x.name != null;
            "${x.name}=${esc x.value}"
          ))
          (builtins.concatStringsSep "\n")
        ]}

        ${lib.pipe srcFile [
          builtins.readFile
          (lib.splitString "\n")
          lib.tail # Cut off the shebang (fixes usage info generator)
          (builtins.concatStringsSep "\n")
        ]}
      '';
    };

  tmuxed-alacritty-new =
    mkScript "new" ./tmuxed-alacritty-new.sh {};
  tmuxed-alacritty-attach =
    mkScript "attach" ./tmuxed-alacritty-attach.sh {};
  tmuxed-alacritty-nuke =
    mkScript "nuke" ./tmuxed-alacritty-nuke.sh {};

  # You are supposed to pass `{ TMUXED_ALACRITTY_EXE = tmuxed-alacritty-new; }`
  # to this one. Or a specific wrapper for `tmuxed-alacritty-new`, like one
  # bound to a colorscheme and/or font.
  tmuxed-alacritty-new-prompt =
    mkScript "new-prompt" ./tmuxed-alacritty-new-prompt.sh;
in

{
  inherit
    tmuxed-alacritty-new
    tmuxed-alacritty-attach
    tmuxed-alacritty-nuke
    tmuxed-alacritty-new-prompt
    ;
}
