# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ callPackage
, lib
, bash
, pulseaudio
, gnome3 # ‘gpaste’ ‘networkmanagerapplet’
, xlibs # ‘xsetroot’

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}
, __input-setup ? callPackage ./input-setup.nix { inherit __nix-utils; }
, __autolock ? callPackage ./autolock.nix { inherit __nix-utils; }
, __picom ? (callPackage ./picom.nix { inherit __nix-utils; }).run-picom

# Build options
, # System config (e.g. self-reference) to extract machine host name.
  # Set to “null” to use in Nix REPL.
  systemConfig
}:
assert lib.isDerivation __input-setup;
assert lib.isDerivation __autolock;
assert lib.isDerivation __picom;
let
  inherit (__nix-utils) esc writeCheckedExecutable nameOfModuleFile shellCheckers;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  # Name is executable name and value is a derivation that provides that executable
  dependencies = {
    bash = bash;
    pactl = pulseaudio;
    gpaste-client = gnome3.gpaste;
    nm-applet = gnome3.networkmanagerapplet;
    xsetroot = xlibs.xsetroot;

    ${__input-setup.name} = __input-setup;
    ${__autolock.name} = __autolock;
    ${__picom.name} = __picom;
  };

  executables = builtins.mapAttrs (n: v: "${dependencies.${n}}/bin/${n}") dependencies;

  checkPhase =
    builtins.concatStringsSep "\n"
      (map shellCheckers.fileIsExecutable (builtins.attrValues executables));

  hostName = systemConfig.networking.hostName or null;
  rw-wenzel-nixos-laptop = callPackage ../hardware/rw-wenzel-nixos-laptop.nix {};
in
writeCheckedExecutable name checkPhase ''
  #! ${executables.bash}
  exec <&- &>/dev/null

  # audio
  ${esc executables.pactl} stat # starting pulseaudio local user server

  # displays
  SCREENLAYOUT=~/.screenlayout/default.sh
  if [[ -f $SCREENLAYOUT && -x $SCREENLAYOUT ]]; then
    if "$SCREENLAYOUT"; then sleep 1s; fi
  fi
  ${
    if hostName != rw-wenzel-nixos-laptop.networking.hostName
    then esc executables.${__picom.name}
    else ""
  }
  if [[ -f ~/.fehbg ]]; then . ~/.fehbg & fi

  ${esc executables.${__input-setup.name}}
  ${esc executables.${__autolock.name}}
  ${esc executables.gpaste-client} & # starting local gpaste daemon
  ${esc executables.nm-applet} & # starting system tray network manager applet

  ${esc executables.xsetroot} -cursor_name left_ptr # default cursor on an empty workspace

  exit 0 # prevent returning exit status of the latest command
''
