args@{ config, options, pkgs, ... }:
let
  inherit (import ./constants.nix)
    wenzelUserName
    rawdevinputGroupName backlightcontrolGroupName jackaudioGroupName audioGroupName;

  sources = import nix/sources.nix;
  nix-utils = pkgs.callPackage sources.nix-utils {};
  inherit (nix-utils) esc;

  grant-access-to-input-devices = import utils/grant-access-to-input-devices { inherit pkgs; };
  laptop-backlight              = import utils/laptop-backlight              { inherit pkgs; };

  my-packages = import ./my-packages.nix args;
  inherit (my-packages.my-apps) wenzels-bash;
in
{
  imports = [
    (import "${sources.home-manager}/nixos")
    my-packages.configuration
    ./gui.nix
    ./fonts.nix
    ./opengl.nix
    ./boot.nix
    ./network.nix
    ./machine-specific.nix
  ] ++ (
    let path = ./secret.nix; in
    if builtins.pathExists path then [path] else []
  ) ++ (
    let path = ./machine-specific.secret.nix; in
    if builtins.pathExists path then [path] else []
  );

  system = {
    stateVersion = "20.09";
    fsPackages = [pkgs.xfsprogs.bin];
  };

  nix.nixPath =
    options.nix.nixPath.default ++
    [ "nixpkgs-overlays=/etc/nixos/overlays-compat/" ];

  nixpkgs.overlays = import ./overlays;

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
    bash.enableCompletion = true;
    zsh.enable = true;
    zsh.enableCompletion = true;

    # TODO configure, see https://framagit.org/mpo/era-configuration-nix
    # chromium = {};
  };

  sound.enable = true;

  hardware = {
    pulseaudio = {
      enable = true;
      support32Bit = true;
      package = pkgs.pulseaudio.override { jackaudioSupport = true; };
    };
  };

  services = {
    printing.enable = false; # CUPS to print documents (have no printer yet)
    upower.enable = true;

    # see also https://nixos.wiki/wiki/JACK
    # jack = {
    #   jackd.enable = true;
    #   # alsa.enable = true; # support ALSA-only programs via ALSA JACK PCM plugin
    #   # loopback.enable = true; # support ALSA-only programms via loopback device (e.g. Steam)
    # };
  };

  users = {
    users.${wenzelUserName} = {
      uid = 1989;
      isNormalUser = true;
      group = wenzelUserName;
      shell = wenzels-bash;

      extraGroups = [
        "users"
        "wheel"
        "networkmanager"
        rawdevinputGroupName
        backlightcontrolGroupName
        "docker"
        "vboxusers"
        jackaudioGroupName
        audioGroupName
      ];
    };

    groups = {
      ${wenzelUserName}.gid = 1989;
      ${rawdevinputGroupName}.gid = 500;
      ${backlightcontrolGroupName}.gid = 501;
    };
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
        rootSuidGroup grant-access-to-input-devices rawdevinputGroupName //
        rootSuidGroup laptop-backlight backlightcontrolGroupName;

    pam.loginLimits = [
      { domain = "@${audioGroupName}"; item = "memlock"; type = "-";    value = "unlimited"; }
      { domain = "@${audioGroupName}"; item = "rtprio";  type = "-";    value = "99";        }
      { domain = "@${audioGroupName}"; item = "nofile";  type = "soft"; value = "99999";     }
      { domain = "@${audioGroupName}"; item = "nofile";  type = "hard"; value = "99999";     }
    ];
  };

  home-manager.users.${wenzelUserName} = {
    programs.git = {
      enable = true;
      userName = "Viacheslav Lotsmanov";
      userEmail = "lotsmanov89@gmail.com";

      signing = {
        # Also known as ‘D276FF7467007335’ (expires: 2023-03-16)
        key = "EE595E29BB5BF2B35ED23F1CD276FF7467007335";
        signByDefault = true;
      };
    };

    home.file.".bashrc".text = ''
      . ${esc wenzels-bash.history-settings-file-path}
    '';
  };
}
