args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, ...
}:
let
  utils = import ../utils args;
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;

  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  dash = "${pkgs.dash}/bin/dash";
  picom = "${pkgs.picom}/bin/picom";
  pkill = "${pkgs.procps}/bin/pkill";

  checkPhase = ''
    ${utils.bash.checkFileIsExecutable dash}
    ${utils.bash.checkFileIsExecutable picom}
    ${utils.bash.checkFileIsExecutable pkill}
  '';

  killPicom = ''${esc pkill} -x -U "$USER" -- ${esc (baseNameOf picom)} 2>/dev/null'';
  restartFeh = "if [ -f ~/.fehbg ]; then . ~/.fehbg & fi";

  run-picom = writeCheckedExecutable "run-${name}" checkPhase ''
    #! ${dash}
    exec <&-

    if [ -z "$1" ]; then ${killPicom}; fi

    # --blur-kern 7x7box
    # --blur-kern 11x11gaussian
    # blur='--blur-background --blur-background-fixed --blur-kern 7x7box'

    # --active-opacity 0.9

    ${esc picom} \
      --dbus \
      --backend glx \
      -c \
      -o 0.3 \
      -m 0.9 \
      --xinerama-shadow-crop \
      $blur \
      &

    ${restartFeh}
  '';

  no-picom = writeCheckedExecutable "no-${name}" checkPhase ''
    #! ${dash}
    exec <&-

    ${killPicom}
    sleep 1
    ${restartFeh}
  '';
in
{
  inherit name run-picom no-picom checkPhase;
}
