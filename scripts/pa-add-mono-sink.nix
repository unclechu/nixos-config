# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ callPackage
, writeText
, dash
, pulseaudio

# Overridable dependencies
, executable-dependencies ? callPackage ../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../utils/mk-generic-script.nix {}
}:

let
  name = "pa-add-mono-sink";

  e = executable-dependencies {
    dash = dash;
    pacmd = pulseaudio;
  };

  src = writeText "${name}-source" ''
    #! /usr/bin/env dash
    # TODO choose sound device, maybe implement a TUI, for instance using "whiptail"
    MASTER='alsa_output.pci-0000_0b_00.3.analog-stereo'

    ${e.s.pacmd} load-module module-remap-sink \
      master="$MASTER" \
      sink_name=mono sink_properties="device.description='Mono'" \
      channels=2 channel_map=mono,mono
  '';
in

mk-generic-script {
  inherit name src e;
  dontAddDependencies = true;
  buildInputs = [ e.executables.dash ];
}
