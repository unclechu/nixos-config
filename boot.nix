{ ... }:
{
  boot = {
    loader = {
      systemd-boot.enable = true;

      grub = {
        enable = true;
        version = 2;
        device = "nodev";
        efiSupport = true;
        enableCryptodisk = true;
      };

      efi = {
        canTouchEfiVariables = true;
        efiSysMountPoint = "/boot/efi";
      };
    };

    initrd = {
      availableKernelModules = [
        "xhci_pci" "ehci_pci" "ahci" "usbhid" "usb_storage" "sd_mod"
        "xfs" "crc32c"
        "nvme"
      ];

      # kernelModules = [];
    };

    # kernelModules = [];
    # extraModulePackages = [];
  };
}