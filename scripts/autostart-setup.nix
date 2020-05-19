args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, input-setup
, autolock
, picom
, ...
}:
let
  utils = import ../utils args;
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;

  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.bash;

  bash = "${pkgs.bash}/bin/bash";
  input-setup-exe = "${input-setup}/bin/${input-setup.name}";
  autolock-exe = "${autolock}/bin/${autolock.name}";
  pactl = "${pkgs.pulseaudio}/bin/pactl";
  gpaste-client = "${pkgs.gnome3.gpaste}/bin/gpaste-client";
  nm-applet = "${pkgs.gnome3.networkmanagerapplet}/bin/nm-applet";
  xsetroot = "${pkgs.xlibs.xsetroot}/bin/xsetroot";
  picom-exe = "${picom}/bin/${picom.name}";

  checkPhase = ''
    ${utils.bash.checkFileIsExecutable bash}
    ${utils.bash.checkFileIsExecutable input-setup-exe}
    ${utils.bash.checkFileIsExecutable autolock-exe}
    ${utils.bash.checkFileIsExecutable pactl}
    ${utils.bash.checkFileIsExecutable gpaste-client}
    ${utils.bash.checkFileIsExecutable nm-applet}
    ${utils.bash.checkFileIsExecutable xsetroot}
    ${utils.bash.checkFileIsExecutable picom-exe}
  '';

  pkg = writeCheckedExecutable name checkPhase ''
    #! ${bash}
    exec <&- &>/dev/null

    ${esc pactl} stat # starting pulseaudio local user server

    SCREENLAYOUT=~/.screenlayout/default.sh
    if [[ -f $SCREENLAYOUT && -x $SCREENLAYOUT ]]; then "$SCREENLAYOUT"; fi

    ${esc input-setup-exe}
    ${esc picom-exe}
    if [[ -f ~/.fehbg ]]; then . ~/.fehbg & fi

    ${esc autolock-exe}

    ${esc gpaste-client} & # starting local gpaste daemon
    ${esc nm-applet} &

    ${esc xsetroot} -cursor_name left_ptr # default cursor on an empty workspace

    exit 0 # prevent returning exit status of the latest command
  '';
in
{
  inherit name pkg checkPhase;
}
