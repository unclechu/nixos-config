# This file is supposed to be symlinked as "machine-specific.nix"
# and imported in "configuration.nix".
{ pkgs ? import <nixpkgs> {}
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

    # FIXME use latest when this error is fixed:
    #       error: Package ‘virtualbox-modules-6.1.14-5.9.11’ in
    #       /nix/store/nbmlij2phd1fk639qbv0ls6wkgpzshl9-nixos/nixos/pkgs/os-specific/linux/virtualbox/default.nix:20
    #       is marked as broken, refusing to evaluate.
    # kernelPackages = pkgs.linuxPackages_latest;
    kernelPackages = pkgs.linuxPackages_5_4;
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
