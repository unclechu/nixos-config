# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# This file is supposed to be symlinked as "machine-specific.nix"
# and imported in "configuration.nix".
{ pkgs, ... }:

let
  swap = {
    swapDevices = [
      {
        device = "/swapfile";
        size = 8192; # 8 GiB
      }
    ];

    zramSwap = {
      enable = false; # I have enough RAM, turned off
      memoryPercent = 100 / 5;
    };

    boot.kernel.sysctl = {
      # Default is 60.
      # Lower means “avoid swap unless memory pressure is high”.
      # Use as less swap as possible.
      "vm.swappiness" = 1;
    };
  };

  graphics = {
    my-boot-attributes.kernelParams = [
      "radeon.cik_support=0"
      "amdgpu.cik_support=1"
      "amdgpu.dc=1"
      "amdgpu.ppfeaturemask=0xffffffff" # allows to adjust clocks and voltages via sysfs
    ];

    # Need some non-free microcodes for "amdgpu"
    hardware.enableRedistributableFirmware = true;

    services.xserver = {
      videoDrivers = [ "modesetting" "amdgpu" ];

      deviceSection = ''
        Option "TearFree" "true"
      '';

      screenSection = ''
        DefaultDepth 24
      '';
    };
  };
in

{
  imports = [
    swap
    graphics
  ];

  networking.hostName = "wenzel-silver-laptop";

  my-boot-attributes.kernelModules = [ "kvm-intel" "fuse" ];

  boot.initrd.luks.devices = {
    boot.device = "/dev/disk/by-uuid/7096ef04-5ea3-4c58-8ac0-914f0e8934a9";
    NixOS.device = "/dev/disk/by-uuid/eb1fea47-2000-4aa9-94e9-d12f9801c95d";
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/279e3afd-5660-483a-870a-bd972f6cf323";
      fsType = "xfs";
    };
    "/boot" = {
      device = "/dev/disk/by-uuid/02A3-4F65";
      fsType = "vfat";
    };
    "/boot/efi" = {
      device = "/dev/disk/by-uuid/429C-ED4D";
      fsType = "vfat";
    };
    "/tmp" = {
      device = "tmpfs";
      fsType = "tmpfs";
    };
  };

  nix.settings.max-jobs = 4;
  powerManagement.cpuFreqGovernor = "powersave";
  hardware.cpu.intel.updateMicrocode = true;
}
