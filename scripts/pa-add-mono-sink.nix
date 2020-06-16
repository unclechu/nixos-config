args@{ ... }:
let pkgs-k = "pkgs"; utils-k = "utils"; config-k = "config"; in
assert let k = pkgs-k;  in builtins.hasAttr k args -> builtins.isAttrs args.${k};
assert let k = utils-k; in builtins.hasAttr k args -> builtins.isAttrs args.${k};
let
  pkgs = args.${pkgs-k} or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { ${k} = args.${k}.nixpkgs.${k}; } else {}
  ));

  utils = args.${utils-k} or (import ../nix-utils-pick.nix (
    pkgs.lib.filterAttrs (k: _: k == config-k) args // { inherit pkgs; }
  )).pkg;

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
