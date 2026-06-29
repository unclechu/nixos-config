# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ callPackage
, writeText

, bash
, coreutils
, pulseaudio
, networkmanagerapplet
, gpaste
, xsetroot

# Overridable dependencies
, __input-setup ? callPackage ./input-setup.nix {}
, __autolock ? callPackage ./autolock.nix {}
, __wenzels-picom ? callPackage ./wenzels-picom { inherit systemConfig; }
, __screen-saver ? callPackage ./screen-saver {}
, __pseudo-primary-display ? callPackage ./pseudo-primary-display {}
, __make-i3-runtime-bar-config ? callPackage ../gui/i3/make-i3-runtime-bar-config.nix {}
, executable-dependencies ? callPackage ../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../utils/mk-generic-script.nix {}

# Build options
, # System config (e.g. self-reference) to extract machine host name.
  # Set to “null” to use in Nix REPL.
  systemConfig
}:

let
  name = "autostart-setup";

  hostName = systemConfig.networking.hostName or null;
  wenzel-rusty-chunk = callPackage ../hardware/wenzel-rusty-chunk.nix {};

  e = executable-dependencies {
    bash = bash;
    sleep = coreutils;
    pactl = pulseaudio;
    gpaste-client = gpaste;
    nm-applet = networkmanagerapplet;
    xsetroot = xsetroot;
    input-setup = __input-setup;
    autolock = __autolock;
    run-picom = __wenzels-picom;
    screen-saver = __screen-saver;
    ${__pseudo-primary-display.copyToRuntimeScript.meta.mainProgram} =
      __pseudo-primary-display.copyToRuntimeScript;
    make-i3-runtime-bar-config = __make-i3-runtime-bar-config;
  };

  src = writeText "${name}-source" ''
    #! /usr/bin/env bash
    exec <&- &>/dev/null

    # audio
    ${e.s.pactl} stat # starting pulseaudio local user server

    # displays
    ${e.s.${__pseudo-primary-display.copyToRuntimeScript.meta.mainProgram}}
    SCREENLAYOUT=~/.screenlayout/default.sh
    if [[ -f "$SCREENLAYOUT" && -x "$SCREENLAYOUT" ]]; then
      if "$SCREENLAYOUT"; then ${e.s.sleep} 1s; fi
    fi
    ${
      let exe = e.s.run-picom; in assert builtins.isString exe;
      # Disable autostart of Picom on some of the machines
      if hostName != wenzel-rusty-chunk.networking.hostName
      then exe
      else ""
    }
    if [[ -f ~/.fehbg ]]; then ${e.s.bash} -- ~/.fehbg & disown; fi
    ${e.s.make-i3-runtime-bar-config}

    ${
      let exe = e.s.autolock; in assert builtins.isString exe;
      # This machine is not intended to be “secure”.
      # So this is only would be an annoyance.
      if hostName != wenzel-rusty-chunk.networking.hostName
      then exe
      else ""
    }

    ${
      let exe = e.s.screen-saver; in assert builtins.isString exe;
      # This machine is very old. And its battary drained its capacity to 0 years
      # and years ago. And its turned off when unattended. So there is no point
      # in having a screensaver.
      if hostName == wenzel-rusty-chunk.networking.hostName
      then "${exe} off"
      else ""
    }

    ${e.s.input-setup}
    ${e.s.gpaste-client} & disown # starting local gpaste daemon
    ${e.s.nm-applet} & disown # starting system tray network manager applet

    ${e.s.xsetroot} -cursor_name left_ptr # default cursor on an empty workspace

    exit 0 # prevent returning exit status of the latest command
  '';
in

mk-generic-script {
  inherit name src e;
  dontAddDependencies = true;
  buildInputs = [ e.executables.bash ];
}
