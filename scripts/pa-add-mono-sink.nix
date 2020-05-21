args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, ...
}:
let
  utils = import ../utils args;
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;

  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  dash = "${pkgs.dash}/bin/dash";
  pacmd = "${pkgs.pulseaudio}/bin/pacmd";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable dash}
    ${utils.shellCheckers.fileIsExecutable pacmd}
  '';

  pkg = writeCheckedExecutable name checkPhase ''
    #! ${dash}

    # TODO choose sound device, maybe implement a TUI, for instance using "whiptail"
    MASTER='alsa_output.pci-0000_0b_00.3.analog-stereo'

    ${esc pacmd} load-module module-remap-sink \
      master="$MASTER" \
      sink_name=mono sink_properties="device.description='Mono'" \
      channels=2 channel_map=mono,mono
  '';
in
{
  inherit name pkg checkPhase;
}
