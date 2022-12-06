# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# This file is supposed to be symlinked as "machine-specific.nix"
# and imported in "configuration.nix".
{ pkgs, ... }:
{
  boot = {
    initrd = {
      luks.devices = {
        boot.device  = "/dev/disk/by-uuid/1494e80a-4e1f-4b73-8dae-09197454fb22";
        NixOS.device = "/dev/disk/by-uuid/b5b68fd7-2128-4fb4-bd9f-e79531af7208";

        # Not used in favor of ZRAM swap
        # swap.device = "/dev/disk/by-uuid/89e71b83-68c6-490e-9468-89a31bf4ea3c";
      };
    };

    kernelModules = [ "kvm-intel" "fuse" ];
    kernelParams = [];
    kernelPackages = pkgs.linuxPackages_latest;
  };

  zramSwap = {
    enable = true;
    memoryPercent = 100 / 8;
  };

  fileSystems = {
    "/" = {
      device = "/dev/mapper/NixOS";
      fsType = "xfs";
    };
    "/boot" = {
      device = "/dev/disk/by-uuid/0577-5753";
      fsType = "vfat";
    };
    "/boot/efi" = {
      device = "/dev/disk/by-uuid/E381-A80E";
      fsType = "vfat";
    };
    "/tmp" = {
      device = "tmpfs";
      fsType = "tmpfs";
    };
  };

  swapDevices = [
    # Not used in favor of ZRAM swap
    # { device = "/dev/mapper/swap"; }
  ];

  networking.hostName = "rw-wenzel-nixos-laptop";

  services.xserver = {
    videoDrivers = [ "intel" ]; # "modesetting"

    deviceSection = ''
      Option "TearFree" "true"
    '';
  };

  hardware.enableRedistributableFirmware = true;

  nix.settings.max-jobs = 8;
  powerManagement.cpuFreqGovernor = "powersave";
}
