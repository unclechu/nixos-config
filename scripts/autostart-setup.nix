# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ callPackage
, lib
, bash
, pulseaudio
, networkmanagerapplet
, gpaste
, xorg # ‘xsetroot’

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}
, __input-setup ? callPackage ./input-setup.nix { inherit __nix-utils; }
, __autolock ? callPackage ./autolock.nix { inherit __nix-utils; }
, __picom ? (callPackage ./picom.nix { inherit __nix-utils systemConfig; }).run-picom
, __screen-saver ? callPackage ./screen-saver {}

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
    gpaste-client = gpaste;
    nm-applet = networkmanagerapplet;
    xsetroot = xorg.xsetroot;

    ${__input-setup.name} = __input-setup;
    ${__autolock.name} = __autolock;
    ${__picom.name} = __picom;
    ${__screen-saver.name} = __screen-saver;
  };

  executables = builtins.mapAttrs (n: v: "${dependencies.${n}}/bin/${n}") dependencies;

  checkPhase =
    builtins.concatStringsSep "\n"
      (map shellCheckers.fileIsExecutable (builtins.attrValues executables));

  hostName = systemConfig.networking.hostName or null;
  rw-wenzel-nixos-laptop = callPackage ../hardware/rw-wenzel-nixos-laptop.nix {};
  wenzel-silver-laptop = callPackage ../hardware/wenzel-silver-laptop.nix {};
  wenzel-rusty-chunk = callPackage ../hardware/wenzel-rusty-chunk.nix {};
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
    # Disable autostart of Picom on some of the machines
    if hostName != wenzel-silver-laptop.networking.hostName
    && hostName != wenzel-rusty-chunk.networking.hostName
    then esc executables.${__picom.name}
    else ""
  }
  if [[ -f ~/.fehbg ]]; then . ~/.fehbg & fi

  ${
    # This machine is not intended to be “secure”.
    # So this is only would be an annoyance.
    if hostName != wenzel-rusty-chunk.networking.hostName
    then esc executables.${__autolock.name}
    else ""
  }

  ${
    # This machine is very old. And its battary drained its capacity to 0 years
    # and years ago. And its turned off when unattended. So there is no point
    # in having a screensaver.
    if hostName == wenzel-rusty-chunk.networking.hostName
    then esc executables.${__screen-saver.name} + " off"
    else ""
  }

  ${esc executables.${__input-setup.name}}
  ${esc executables.gpaste-client} & # starting local gpaste daemon
  ${esc executables.nm-applet} & # starting system tray network manager applet

  ${esc executables.xsetroot} -cursor_name left_ptr # default cursor on an empty workspace

  exit 0 # prevent returning exit status of the latest command
''
