args@{ ... }:
assert let k = "pkgs";  in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
assert let k = "utils"; in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
let
  pkgs = args.pkgs or (import <nixpkgs> {
    config = let k = "config"; in
      if builtins.hasAttr k args then {} else args."${k}".nixpkgs.config;
  });

  utils = args.utils or (import ../nix-utils-pick.nix args).pkg;
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
