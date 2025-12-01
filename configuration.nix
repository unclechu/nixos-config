# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
args@{ config, options, pkgs, lib, ... }:
let
  inherit (import ./constants.nix)
    wenzelUserName
    systemProfile
    rawdevinputGroupName
    backlightcontrolGroupName
    cpumodecontrolGroupName
    jackaudioGroupName
    audioGroupName
    ;

  grant-access-to-input-devices = pkgs.callPackage utils/grant-access-to-input-devices {};
  laptop-backlight = pkgs.callPackage utils/laptop-backlight {};
  cpu-mode-switch = pkgs.callPackage utils/cpu-mode-switch {};

  my-packages = import ./my-packages.nix args;
in
{
  imports = [
    my-packages.configuration
    ./gui.nix
    ./fonts.nix
    ./opengl.nix
    ./boot.nix
    ./network.nix
    ./user-specific.nix
    ./machine-specific.nix
    ./qt-apps-crashing-fix.nix

    # Custom options
    custom-options/unfree-predicates.nix
    custom-options/whitelisted-insecure-packages.nix
  ] ++ (
    let path = ./secret.nix; in
    lib.optional (builtins.pathExists path) path
  ) ++ (
    let path = ./machine-specific.secret.nix; in
    lib.optional (builtins.pathExists path) path
  );

  system = {
    stateVersion = "20.09";
    fsPackages = [pkgs.xfsprogs.bin];
  };

  nix = {
    nixPath =
      options.nix.nixPath.default
      ++ [ "nixpkgs-overlays=/etc/nixos/overlays-compat/" ];

    extraOptions = ''
      experimental-features = nix-command flakes
    '';
  };

  whitelistedInsecurePackages = [
    # Temporary exception for some of the Matrix clients.
    "olm-3.2.16"
  ];

  nixpkgs = {
    config = {
      permittedInsecurePackages = config.whitelistedInsecurePackages;
      allowUnfreePredicate = pkg:
        builtins.foldl' (acc: f: f pkg || acc) false config.unfreePredicates;
    };

    overlays = (import ./overlays) ++ [
      # A hack to make system profile name available in all of the modules.
      # It’s available as “pkgs.systemProfile” but only inside this NixOS configuration
      # (not available in <nixpkgs> channel).
      (self: super: { systemProfile = systemProfile.default; })
    ];
  };

  i18n.defaultLocale = "en_US.UTF-8";
  console.keyMap = "us";

  time.timeZone = "Europe/Helsinki";

  # shellInit = ''
  #   export FOO=bar
  # '';

  environment = {
    variables = {
      EDITOR = "nvim";
      TERMINAL = "termite";
    };

    sessionVariables = {
      LV2_PATH = "/run/current-system/sw/lib/lv2";
    };
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs = {
    seahorse.enable = true;
    dconf.enable = true;
    gpaste.enable = true;
    bash.completion.enable = true;
    zsh.enable = true;
    zsh.enableCompletion = true;

    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryPackage = pkgs.pinentry-gnome3;
    };

    # TODO configure, see https://framagit.org/mpo/era-configuration-nix
    # chromium = {};
  };

  services = {
    printing.enable = false; # CUPS to print documents (have no printer yet)
    upower.enable = true; # Getting info about battery charge via D-Bus
    gvfs.enable = true; # Mount, trash, and other stuff
    tumbler.enable = true; # Thumbnails for images
    ratbagd.enable = true; # Gaming mouse configuration daemon

    pulseaudio = {
      enable = true;
      support32Bit = true; # Support 32-bit applications just in case
      # Usually I’m permanently running JACK daemon forwarding PulseAudio into it
      package = pkgs.pulseaudio.override { jackaudioSupport = true; };
    };
    # It’s supposed to be `false` by default but somehow it’s not?
    #
    # I was getting this error if this is not set here to `false`:
    # > Using PipeWire as the sound server conflicts with PulseAudio.
    # > This option requires `hardware.pulseaudio.enable` to be set to false
    pipewire.enable = false;

    # See also https://nixos.wiki/wiki/JACK
    # I’m starting JACK manually via “jack_control start” or QjackCtl
    # jack = {
    #   jackd.enable = true;
    #   # alsa.enable = true; # support ALSA-only programs via ALSA JACK PCM plugin
    #   # loopback.enable = true; # support ALSA-only programms via loopback device (e.g. Steam)
    # };
  };

  security = {
    wrappers =
      let
        rootSuidGroup = source: group: {
          ${source.name} = {
            source = source;
            permissions = "u+xs,g+x";
            owner = "root";
            group = group;
          };
        };
      in
        rootSuidGroup grant-access-to-input-devices rawdevinputGroupName
        //
        rootSuidGroup laptop-backlight backlightcontrolGroupName
        //
        rootSuidGroup cpu-mode-switch cpumodecontrolGroupName
        ;

    pam.loginLimits = [
      { domain = "@${audioGroupName}"; item = "memlock"; type = "-";    value = "unlimited"; }
      { domain = "@${audioGroupName}"; item = "rtprio";  type = "-";    value = "99";        }
      { domain = "@${audioGroupName}"; item = "nofile";  type = "soft"; value = "99999";     }
      { domain = "@${audioGroupName}"; item = "nofile";  type = "hard"; value = "99999";     }
    ];
  };
}
