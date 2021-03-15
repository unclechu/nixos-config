{ pkgs, ... }:
{
  hardware.opengl =
    let
      extras = [
        "mesa"
        "vaapiVdpau"
        "vaapiIntel"
        "libvdpau-va-gl"
        "libva"
      ];
    in
      {
        enable = true;
        driSupport = true;
        driSupport32Bit = true;
        extraPackages = builtins.map (name: pkgs.${name}) extras;
        extraPackages32 = builtins.map (name: pkgs.pkgsi686Linux.${name}) extras;
      };
}
