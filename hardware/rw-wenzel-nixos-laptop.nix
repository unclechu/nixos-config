# This file is supposed to be symlinked as "machine-specific.nix"
# and imported in "configuration.nix".
args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, ...
}:
{
  boot = {
    initrd = {
      luks.devices = {
        boot.device  = "/dev/disk/by-uuid/1494e80a-4e1f-4b73-8dae-09197454fb22";
        swap.device  = "/dev/disk/by-uuid/89e71b83-68c6-490e-9468-89a31bf4ea3c";
        NixOS.device = "/dev/disk/by-uuid/b5b68fd7-2128-4fb4-bd9f-e79531af7208";
      };
    };

    kernelModules = [ "kvm-intel" "fuse" ];
    kernelParams = [];
    kernelPackages = pkgs.linuxPackages_latest;
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
    { device = "/dev/mapper/swap"; }
  ];

  networking.hostName = "rw-wenzel-nixos-laptop";

  services.xserver = {
    videoDrivers = [ "intel" ]; # "modesetting"

    deviceSection = ''
      Option "TearFree" "true"
    '';
  };

  hardware.enableRedistributableFirmware = true;

  nix.maxJobs = 8;
  powerManagement.cpuFreqGovernor = "powersave";
}
