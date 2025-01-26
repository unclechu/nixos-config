{ config, pkgs, ... }:
{
  networking.hostName = "wenzel-silver-laptop";

  boot = {
    initrd = {
      luks.devices = {
        boot.device = "/dev/disk/by-uuid/7096ef04-5ea3-4c58-8ac0-914f0e8934a9";
        NixOS.device = "/dev/disk/by-uuid/eb1fea47-2000-4aa9-94e9-d12f9801c95d";
      };
    };

    kernelModules = [ "kvm-intel" "fuse" ];

    kernelParams = [
      "radeon.cik_support=0"
      "amdgpu.cik_support=1"
      "amdgpu.dc=1"
      "amdgpu.ppfeaturemask=0xffffffff" # allows to adjust clocks and voltages via sysfs
    ];

    kernelPackages =
      (import ../fix-cpupower-build-failure.nix) pkgs pkgs.linuxPackages_latest;
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

  swapDevices = [ ];

  zramSwap = {
    enable = false; # I have enough RAM, turned off
    memoryPercent = 100 / 5;
  };

  nix.settings.max-jobs = 4;
  powerManagement.cpuFreqGovernor = "powersave";
  hardware.enableRedistributableFirmware = true; # Need some non-free microcodes for "amdgpu"
  hardware.cpu.intel.updateMicrocode = config.hardware.enableRedistributableFirmware;

  services.xserver = {
    videoDrivers = [ "modesetting" "amdgpu" ];

    deviceSection = ''
      Option "TearFree" "true"
    '';

    screenSection = ''
      DefaultDepth 24
    '';
  };
}
