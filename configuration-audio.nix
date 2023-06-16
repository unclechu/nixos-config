# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
#
# The optimization tweaks are taken from this page:
# https://nixos.wiki/wiki/JACK
#
args@{ pkgs, lib, ... }:
let
  inherit (import ./constants.nix) systemProfile;
  machine-specific = import ./machine-specific.nix args;
  inherit (machine-specific) boot;
  sources = import nix/sources.nix;
  rtirq = pkgs.callPackage "${sources.musnix}/pkgs/os-specific/linux/rtirq/default.nix" {};

  # Disable some known services that can be running in background
  turnOffs = {
    virtualisation.libvirtd.enable = lib.mkForce false;
    virtualisation.docker.enable = lib.mkForce false;
    virtualisation.virtualbox.host.enable = lib.mkForce false;

    services.gvfs.enable = lib.mkForce false;
    services.avahi.enable = lib.mkForce false;
    services.tor.enable = lib.mkForce false;
    services.tor.client.enable = lib.mkForce false;

    # hardware.bluetooth.enable = lib.mkForce false;
    # services.blueman.enable = lib.mkForce false;
  };
in
{
  imports = [
    ./configuration.nix
    turnOffs
  ];

  nixpkgs.overlays = import ./overlays ++ [
    # A hack to make system profile name available in all of the modules.
    # It’s available as “pkgs.systemProfile” but only inside this NixOS configuration
    # (not available in <nixpkgs> channel).
    (self: super: { systemProfile = systemProfile.audio; })
  ];

  boot = {
    kernel.sysctl = { "vm.swappiness" = 10; "fs.inotify.max_user_watches" = 524288; };
    kernelModules = lib.mkForce (boot.kernelModules ++ [ "snd-seq" "snd-rawmidi" ]);
    kernelParams = lib.mkForce (boot.kernelParams ++ [ "threadirq" ]);
    kernelPackages = lib.mkForce pkgs.linuxPackages-rt_latest;

    postBootCommands = ''
      echo 2048 > /sys/class/rtc/rtc0/max_user_freq
      echo 2048 > /proc/sys/dev/hpet/max-user-freq
      # I don’t see my sound card in the output of “lspci | grep -i audio”
      # See also: https://github.com/musnix/musnix/blob/7fb04384/modules/base.nix#L58-L59
      # setpci -v -d *:* latency_timer=b0
      # setpci -v -s $00:1b.0 latency_timer=ff
    '';
  };

  powerManagement.cpuFreqGovernor = lib.mkForce "performance";

  # Didn’t get why would I need it yet
  # fileSystems."/" = { options = "noatime errors=remount-ro"; };

  services = {
    udev = {
      packages = [
        # Uncomment for FireWire audio interface support
        # pkgs.ffado
      ];

      extraRules = ''
        KERNEL=="rtc0", GROUP="audio"
        KERNEL=="hpet", GROUP="audio"
      '';
    };

    cron.enable = false;
  };

  environment = {
    # See also https://github.com/musnix/musnix/blob/7fb04384/modules/rtirq.nix#L82
    systemPackages = [ rtirq ];

    # See https://github.com/musnix/musnix/blob/7fb04384/modules/rtirq.nix#L9
    etc."rtirq.conf".source = pkgs.writeText "rtirq.conf" ''
      RTIRQ_NAME_LIST="snd usb i8042" # What’s this supposed to refer to?

      RTIRQ_PRIO_HIGH=90 # Highest priority
      RTIRQ_PRIO_DECR=5  # Priority decrease step
      RTIRQ_PRIO_LOW=51  # Lowest priority
      RTIRQ_RESET_ALL=0  # Whether to reset all IRQ threads to SCHED_OTHER

      RTIRQ_NON_THREADED="rtc snd" # Which services should be NOT threaded

      # Process names which will be forced to the
      # highest realtime priority range (99-91)
      # RTIRQ_HIGH_LIST=""
    '';
  };
}
