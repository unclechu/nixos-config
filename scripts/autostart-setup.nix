let sources = import ../nix/sources.nix; in
{ pkgs
, nix-utils ? pkgs.callPackage sources.nix-utils {}

, input-setup ? pkgs.callPackage ./input-setup.nix {}
, autolock    ? import ./autolock.nix { inherit pkgs; }
, picom       ? (import ./picom.nix { inherit pkgs; }).run-picom

, # System config (e.g. self-reference) to extract machine host name.
  # Set to “null” to use in Nix REPL.
  config
}:
assert pkgs.lib.isDerivation input-setup;
assert pkgs.lib.isDerivation autolock;
assert pkgs.lib.isDerivation picom;
let
  inherit (nix-utils) esc writeCheckedExecutable nameOfModuleFile;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.bash;
  bash = "${pkgs.bash}/bin/bash";
  pactl = "${pkgs.pulseaudio}/bin/pactl";
  gpaste-client = "${pkgs.gnome3.gpaste}/bin/gpaste-client";
  nm-applet = "${pkgs.gnome3.networkmanagerapplet}/bin/nm-applet";
  xsetroot = "${pkgs.xlibs.xsetroot}/bin/xsetroot";
  input-setup-exe = "${input-setup}/bin/${input-setup.name}";
  autolock-exe = "${autolock}/bin/${autolock.name}";
  picom-exe = "${picom}/bin/${picom.name}";

  hostName = config.networking.hostName or null;
  rw-wenzel-nixos-laptop = import ../hardware/rw-wenzel-nixos-laptop.nix { inherit pkgs; };

  checkPhase = ''
    ${nix-utils.shellCheckers.fileIsExecutable bash}
    ${nix-utils.shellCheckers.fileIsExecutable pactl}
    ${nix-utils.shellCheckers.fileIsExecutable gpaste-client}
    ${nix-utils.shellCheckers.fileIsExecutable nm-applet}
    ${nix-utils.shellCheckers.fileIsExecutable xsetroot}
    ${nix-utils.shellCheckers.fileIsExecutable input-setup-exe}
    ${nix-utils.shellCheckers.fileIsExecutable autolock-exe}
    ${nix-utils.shellCheckers.fileIsExecutable picom-exe}
  '';
in
writeCheckedExecutable name checkPhase ''
  #! ${bash}
  exec <&- &>/dev/null

  # audio
  ${esc pactl} stat # starting pulseaudio local user server

  # displays
  SCREENLAYOUT=~/.screenlayout/default.sh
  if [[ -f $SCREENLAYOUT && -x $SCREENLAYOUT ]]; then
    if "$SCREENLAYOUT"; then sleep 1s; fi
  fi
  ${
    if hostName != rw-wenzel-nixos-laptop.networking.hostName
    then picom-exe
    else ""
  }
  if [[ -f ~/.fehbg ]]; then . ~/.fehbg & fi

  ${esc input-setup-exe}
  ${esc autolock-exe}
  ${esc gpaste-client} & # starting local gpaste daemon
  ${esc nm-applet} & # starting system tray network manager applet

  ${esc xsetroot} -cursor_name left_ptr # default cursor on an empty workspace

  exit 0 # prevent returning exit status of the latest command
''
