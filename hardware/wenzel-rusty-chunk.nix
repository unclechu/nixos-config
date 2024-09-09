# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# This file is supposed to be symlinked as "machine-specific.nix"
# and imported in "configuration.nix".
let
  inherit (import ../constants.nix) wenzelUserName;
  bootDisk = "/dev/sda";
  rootPartitionUUID = "41015b40-f70e-40bc-9cdb-fed0595b442f";
  swapPartitionUUID = "0fa4a156-b413-4860-8d2d-5402ae52fe6c";
in
{ config, pkgs, lib, ... }:
{
  boot = {
    # This hardware is pretty old, it doesn't support EFI.
    loader.grub.efiSupport = lib.mkForce false;
    loader.grub.enableCryptodisk = lib.mkForce false;
    loader.grub.device = lib.mkForce bootDisk;

    # Autogenerated list of modules by “nixos-generate-config”.
    initrd.availableKernelModules = [
      "ahci"
      "ohci_pci"
      "ehci_pci"
      "usb_storage"
      "ums_realtek"
      "sd_mod"
      "sr_mod"
    ];

    kernelParams = [];
    kernelModules = ["kvm-amd" "fuse"];
    kernelPackages = pkgs.linuxPackages_latest;
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/${rootPartitionUUID}";
      fsType = "xfs";
    };
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/${swapPartitionUUID}"; }
  ];

  networking.hostName = "wenzel-rusty-chunk";

  services.xserver = {
    # Technically this pretty old machine has AMD GPU chip but I’m not sure
    # if “amdgpu” driver supports it. Or if I even need it for this machine
    # as I’m not planning to do anything that would be heavy on GPU.
    # videoDrivers = ["amdgpu"];

    deviceSection = ''
      Option "TearFree" "true"
    '';
  };

  # Needed for some non-free microcodes for "amdgpu"
  hardware.enableRedistributableFirmware = true;
  hardware.cpu.amd.updateMicrocode = config.hardware.enableRedistributableFirmware;

  nixpkgs.hostPlatform = "x86_64-linux";

  nix.settings.max-jobs = 4;
  powerManagement.cpuFreqGovernor = "powersave";

  services.displayManager.autoLogin.enable = true;
  services.displayManager.autoLogin.user = wenzelUserName;

  home-manager.users.${wenzelUserName}.programs.git.signing.signByDefault =
    lib.mkForce false;
}