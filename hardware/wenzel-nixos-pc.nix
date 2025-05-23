# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# This file is supposed to be symlinked as "machine-specific.nix"
# and imported in "configuration.nix".
{ pkgs, ... }:
{
  boot = {
    initrd = {
      luks.devices = {
        boot.device  = "/dev/disk/by-uuid/a486a786-fb96-4b51-924f-3ef1430abfb7";
        NixOS.device = "/dev/disk/by-uuid/1f7c998f-f5de-4a49-8da2-1e88b057793a";

        # Not used in favor of ZRAM swap
        # swap.device = "/dev/disk/by-uuid/53e0ec63-4122-4ce3-b6e1-8c911417ac76";
      };
    };

    kernelModules = ["kvm-intel" "kvm-amd" "fuse"];
    # kernelParams = ["radeon.si_support=0" "amdgpu.si_support=1" "amdgpu.dc=1"];
    kernelParams = [
      "radeon.cik_support=0"
      "amdgpu.cik_support=1"
      "amdgpu.dc=1"
      "amdgpu.ppfeaturemask=0xffffffff" # allows to adjust clocks and voltages via sysfs
    ];

    kernelPackages =
      (import ../fix-cpupower-build-failure.nix) pkgs pkgs.linuxPackages_latest;
  };

  zramSwap = {
    enable = true;
    memoryPercent = 100 / 5;
  };

  fileSystems = {
    "/" = {
      device = "/dev/mapper/NixOS";
      fsType = "xfs";
    };
    "/boot" = {
      device = "/dev/disk/by-uuid/0ED9-C29E";
      fsType = "vfat";
    };
    "/boot/efi" = {
      device = "/dev/disk/by-uuid/A412-D017";
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

  networking.hostName = "wenzel-nixos-pc";

  # see also https://wiki.archlinux.org/index.php/AMDGPU
  # see also https://github.com/NixOS/nixpkgs/issues/44183
  # see also https://github.com/NixOS/nixpkgs/issues/55407
  # services.xserver.videoDrivers = ["amdgpu" "radeon" "vesa" "modesetting"];
  services.xserver = {
    videoDrivers = ["amdgpu"];

    deviceSection = ''
      Option "TearFree" "true"
      Option "DRI" "3"
      # Option "Accel" "true"
      # Option "AccelMethod" "glamor"
      # Option "ShadowPrimary" "true"
    '';

    screenSection = ''
      DefaultDepth 24
    '';
  };

  # need some non-free microcodes for "amdgpu"
  hardware.enableRedistributableFirmware = true;

  nix.settings.max-jobs = 4;
  powerManagement.cpuFreqGovernor = "performance"; # "powersave"
}
