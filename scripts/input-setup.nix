args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, wenzels-keyboard
, pointer-dell-latitude-laptop-dot
, pointer-dell-latitude-laptop-touchpad
, pointer-logitech-wireless-ambidextrous-small-mouse
, pointer-razor-wired-ambidextrous-mouse
, ...
}:
let
  utils = import ../utils args;
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;

  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.bash;

  bash = "${pkgs.bash}/bin/bash";
  xinput = "${pkgs.xlibs.xinput}/bin/xinput";
  wenzels-keyboard-exe = "${wenzels-keyboard}/bin/wenzels-keyboard";

  pointer-dell-latitude-laptop-dot-exe =
    "${pointer-dell-latitude-laptop-dot}/bin/pointer-dell-latitude-laptop-dot";
  pointer-dell-latitude-laptop-touchpad-exe =
    "${pointer-dell-latitude-laptop-touchpad}/bin/pointer-dell-latitude-laptop-touchpad";
  pointer-logitech-wireless-ambidextrous-small-mouse-exe =
    pointer-logitech-wireless-ambidextrous-small-mouse +
      "/bin/pointer-logitech-wireless-ambidextrous-small-mouse";
  pointer-razor-wired-ambidextrous-mouse-exe =
    "${pointer-razor-wired-ambidextrous-mouse}/bin/pointer-razor-wired-ambidextrous-mouse";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable bash}
    ${utils.shellCheckers.fileIsExecutable xinput}
    ${utils.shellCheckers.fileIsExecutable wenzels-keyboard-exe}
    ${utils.shellCheckers.fileIsExecutable pointer-dell-latitude-laptop-dot-exe}
    ${utils.shellCheckers.fileIsExecutable pointer-dell-latitude-laptop-touchpad-exe}
    ${utils.shellCheckers.fileIsExecutable pointer-logitech-wireless-ambidextrous-small-mouse-exe}
    ${utils.shellCheckers.fileIsExecutable pointer-razor-wired-ambidextrous-mouse-exe}
  '';

  pkg = writeCheckedExecutable name checkPhase ''
    #! ${bash}
    exec <&- &>/dev/null

    # keyboard
    ${esc wenzels-keyboard-exe} &

    # mouse
    ${esc pointer-dell-latitude-laptop-dot-exe} &
    ${pointer-dell-latitude-laptop-touchpad-exe} &
    ${pointer-logitech-wireless-ambidextrous-small-mouse-exe} &
    ${pointer-razor-wired-ambidextrous-mouse-exe} &

    # laptop touchscreen
    ${esc xinput} --map-to-output 'ELAN25B6:00 04F3:0732' eDP1

    exit 0 # prevent returning exit status of the latest command
  '';
in
{
  inherit name pkg checkPhase;
}
