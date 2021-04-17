# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ callPackage
, dash
, pulseaudio

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}
}:
let
  inherit (__nix-utils) esc writeCheckedExecutable nameOfModuleFile shellCheckers;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  # Name is executable name and value is a derivation that provides that executable
  dependencies = {
    dash = dash;
    pacmd = pulseaudio;
  };

  executables = builtins.mapAttrs (n: v: "${dependencies.${n}}/bin/${n}") dependencies;

  checkPhase =
    builtins.concatStringsSep "\n"
      (map shellCheckers.fileIsExecutable (builtins.attrValues executables));
in
writeCheckedExecutable name checkPhase ''
  #! ${executables.dash}

  # TODO choose sound device, maybe implement a TUI, for instance using "whiptail"
  MASTER='alsa_output.pci-0000_0b_00.3.analog-stereo'

  ${esc executables.pacmd} load-module module-remap-sink \
    master="$MASTER" \
    sink_name=mono sink_properties="device.description='Mono'" \
    channels=2 channel_map=mono,mono
''
