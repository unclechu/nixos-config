# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ lib
, callPackage
, bash
, coreutils
, gnused
, gnugrep
, xprop
, xdotool
, tmux
, skim

, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../../utils/mk-generic-script.nix {}
}:
let
  executablesMap = {
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
    xprop = xprop;
    tmux = tmux;
  };

  mkScript = action: srcFile: extraEnvMap: alacrittyPkg:
    let
      e = executable-dependencies (executablesMap // {
        ${alacrittyPkg.meta.mainProgram} = alacrittyPkg;
      });
    in
    mk-generic-script {
      name = "tmuxed-${alacrittyPkg.meta.mainProgram}-${action}";
      src = srcFile;
      inherit e;

      wrapProgramArgs = [
        "--set" "TMUX_EXE" e.b.tmux
        "--set" "ALACRITTY_EXE" e.b.${alacrittyPkg.meta.mainProgram}
        "--set" "SKIM_EXE" e.b.sk
      ] ++ (
        lib.pipe extraEnvMap [
          (x: assert builtins.isAttrs x; x)
          lib.attrsToList
          (map (x:
            assert builtins.match "^[A-Z_]+$" x.name != null;
            ["--set" x.name x.value]
          ))
          lib.flatten
        ]
      );
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
