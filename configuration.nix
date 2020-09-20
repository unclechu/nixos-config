args@{ ... }:
let
  config-k = "config";

  inherit (import ./constants.nix)
    wenzelUserName xkb keyRepeat
    rawdevinputGroupName backlightcontrolGroupName jackaudioGroupName audioGroupName;
in
# WARNING! Never assert "config" argument, nowhere in the modules too, it's recursive self-reference.
#          It refers to the configuration itself, to this module we're just about to construct.
#          Otherwise it will fail with infinite recursion error.
# assert let k = config-k; in builtins.hasAttr k args -> builtins.isAttrs args.${k};
let
  withConfigArgs =
    let k = config-k; in if builtins.hasAttr k args then { ${k} = args.${k}; } else {};

  pkgs = import ./pkgs.nix withConfigArgs;
  utils = (import ./picks/nix-utils.nix (withConfigArgs // { inherit pkgs; })).pkg;
  inherit (utils) esc;
  moduleArgs = withConfigArgs // { inherit pkgs utils; };
  home-manager = import ./picks/home-manager.nix;

  wenzels-i3 = import apps/wenzels-i3.nix moduleArgs;

  grant-access-to-input-devices = (import utils/grant-access-to-input-devices moduleArgs).pkg;
  laptop-backlight              = (import utils/laptop-backlight              moduleArgs).pkg;

  my-packages = import ./my-packages.nix moduleArgs;

  inherit (my-packages.my-apps) wenzels-bash;
in
{
  imports = [
    (import "${home-manager}/nixos")
    my-packages.configuration
    ./machine-specific.nix
  ];

  system = {
    stateVersion = "20.09";
    fsPackages = [pkgs.xfsprogs.bin];
  };

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

  networking = {
    networkmanager.enable = true;
    # wireless.enable = true; # incompatible with networkmanager
    useDHCP = false; # just legacy flag

    # let network manager do the work.
    # if you turn both this and network manager on the network
    # will constantly go up and down in an infinite loop.
    # interfaces.enp3s0.useDHCP = false;

    # proxy = {
    #   default = "http://user:password@proxy:port/";
    #   noProxy = "127.0.0.1,localhost,internal.domain";
    # };

    firewall = {
      enable = true;
      allowedTCPPorts = [
        # 80 443
      ];
      allowedTCPPortRanges = [
        # { from = 8000; to = 8010; }
      ];
    };
  };

  i18n.defaultLocale = "en_US.UTF-8";

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  time.timeZone = "Europe/Helsinki";

  # shellInit = ''
  #   export FOO=bar
  # '';

  environment = {
    variables = {
      EDITOR = "nvim";
      TERMINAL = "termite";

      # XCompose and XIM setup
      XMODIFIERS = "@im=none";
      QT_IM_MODULE = "xim";
      GTK_IM_MODULE = "xim";
    };

    sessionVariables = {
      LV2_PATH = "/run/current-system/sw/lib/lv2";
      GTK_THEME = "Adwaita:dark";
    };

    etc = {
      "xdg/gtk-2.0/gtkrc".text = ''
        gtk-theme-name = "Adwaita-dark"
        gtk-icon-theme-name = "Adwaita"
      '';
      "xdg/gtk-3.0/settings.ini".text = ''
        [Settings]
        gtk-theme-name = Adwaita-dark
        gtk-application-prefer-dark-theme = true
        gtk-icon-theme-name = Adwaita
      '';

      # Qt4
      "xdg/Trolltech.conf".text = ''
        [Qt]
        style=GTK+
      '';
    };
  };

  qt5 = {
    enable = true;
    platformTheme = "gnome";
    style = "adwaita-dark";
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  programs = {
    gnupg.agent = {
      enable = true;
      enableSSHSupport = true;
      pinentryFlavor = "gnome3";
    };

    ssh.knownHosts = {
      "wenzel-nixos-pc.local" = {
        publicKey =
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOmL8yMVM5zEkU3v3BEBdGRKeTUE23SLM0cQpmZ7KsWy";
      };
      "rw-wenzel-nixos-laptop.local" = {
        publicKey =
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINuEudWImNcFDrEfvfUipPBBcpVHj0YsWiqh0qLURPIo";
      };
    };

    seahorse.enable = true;
    dconf.enable = true;
    gpaste.enable = true;
    bash.enableCompletion = true;
    zsh.enable = true;
    zsh.enableCompletion = true;
    tmux.enable = true;

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

    opengl = rec {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;

      extraPackages = [
        pkgs.mesa
        pkgs.vaapiVdpau
        pkgs.vaapiIntel
        pkgs.libvdpau-va-gl
      ];

      extraPackages32 = extraPackages;
    };
  };

  services = {
    openssh = {
      enable = true;
      permitRootLogin = "no";
      passwordAuthentication = false;
      startWhenNeeded = false;
      forwardX11 = true;
    };

    # discover local machines
    avahi = {
      enable = true;
      ipv4 = true;
      ipv6 = true;
      nssmdns = true;

      publish = {
        enable = true;
        userServices = true;
        domain = true;
      };
    };

    udev = {
      extraRules = ''
        # Teensy rules for the Ergodox EZ Original / Shine / Glow
        ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", ENV{ID_MM_DEVICE_IGNORE}="1"
        ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789A]?", ENV{MTP_NO_PROBE}="1"
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789ABCD]?", MODE:="0666"
        KERNEL=="ttyACM*", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", MODE:="0666"

        # STM32 rules for the Planck EZ Standard / Glow
        SUBSYSTEMS=="usb", ATTRS{idVendor}=="0483", ATTRS{idProduct}=="df11", \
            MODE:="0666", \
            SYMLINK+="stm32_dfu"
      '';
    };

    printing.enable = false; # CUPS to print documents (have no printer yet)

    # X11
    xserver = {
      enable = true;
      layout = xkb.layout;
      xkbOptions = xkb.options;

      desktopManager = {
        # default = "none";
        xterm.enable = false;
      };

      displayManager = {
        defaultSession = "none+i3";
        lightdm.enable = true;

        sessionCommands = ''
          ${esc pkgs.xorg.xset}/bin/xset r rate ${esc keyRepeat.delay} ${esc keyRepeat.interval}
        '';
      };

      windowManager.i3 = {
        enable = true;

        configFile = wenzels-i3.configFile {
          inherit (my-packages.my-apps)
            autostart-setup input-setup
            cursor-to-display gpaste-gui pamng screen-backlight;
        };
      };

      libinput.enable = true; # touchpad
    };

    gnome3.gnome-keyring.enable = true;
    upower.enable = true;
    fwupd.enable = true;

    # see also https://nixos.wiki/wiki/JACK
    # jack = {
    #   jackd.enable = true;
    #   # alsa.enable = true; # support ALSA-only programs via ALSA JACK PCM plugin
    #   # loopback.enable = true; # support ALSA-only programms via loopback device (e.g. Steam)
    # };
  };

  virtualisation = {
    docker.enable = true;

    virtualbox.host = {
      enable = true;
      headless = false;
    };
  };

  fonts = {
    enableFontDir = true;
    enableDefaultFonts = true;
    enableGhostscriptFonts = true;

    fonts = [
      pkgs.hack-font
      pkgs.fira-code-symbols
      pkgs.iosevka
      pkgs.nerdfonts
      pkgs.terminus_font # for "place-cursor-at"
    ];

    fontconfig = {
      antialias = true;
      hinting.enable = true;
      subpixel.rgba = "none";

      defaultFonts =
        let
          font = ["Hack"];
        in {
          monospace = font;
          sansSerif = font;
          serif = font;
          emoji = ["Noto Color Emoji"];
        };
    };
  };

  users = {
    users."${wenzelUserName}" = {
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

      openssh.authorizedKeys.keys = [
        # wenzel-nixos-pc
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDL2A7YiBrYwyxx9HTa8YVGeS8et1ae1K7VuwYtqL+pV41GeQdpug7crGbM9U2NQ4zXBxpcV0Wu/Fxa4KcecBuw2PTv8bJO6+38U39iOWtJ6ArxC0dkAPTssVpWw1Bvc61WUvLsP8PtgauZjO18HAOJD62yPmqOgf/IIY9TCAv2QuQD+nSaPVn3TSVUDhBCFM/ZC7I/D0pdgWTkSUKEoNgXF47sVYs14BN8ey87UQd9OK02gbcGUMXaLXGQfa5BezHHiwpyVCnkujpaKkiNOvcmb5bVyhT8bnds2zmEOu6wmFEyiK0Cozb6b7vpnjtKV+U4M+IJzSkV9RjeoJaEN79e0SC1YHusKE5EZlcgvVt5+sR9G1mXlXaqL/g3rMuDy4gECDa0PGSFj6tiYqlk22Pdcx8dzK1yMzroeCD0++umChyRm6xM8sb7vRm+j3ZnRdeSq77VX66q0oJaVeCCXF4BGf+RJgf0SApKfg9Xts0EekZylYyerXw1i4R8eV9WIr4oA7vCTg/QuAa7U0lezDBNjf+cb9eZu4kgyi/Cft06T17DS3t7Nqe2cgsGqfJpnSPTTV6HduLmQAhfcDGbJ5QuzMswUbgCh4ZbMDinBPXRwoCyfQa+L21gcFYd9QkTt6KEuciAd9L8AzyzhhJCid85Ku7DNcRCz7epFwYdkKT+Iw== (none)"
        # rw-wenzel-nixos-laptop
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC/7GGxP+9Y0oiCPFuFWVgYfQV26K4yLbVOWXaVjhl8ydZykgGZ1BHt/IHeq7FpJimwFbCj1JiFi/iAW4oY5yQs5YKR/rJ5zlyUeOfVFZHV1mONN8hyDOyftO/KGGWWPdEB6bLlMyeP2bNjrSulvljp/zHix1be00dfoKA+D9i0haYQX4kJT1H3j9Ouf4Ea5kQmxLlksQIwEOTIsyi/UWU2U9NmBBi88oQwluIAZjOPUeijq3jnGti0yWPHnXqoD0ssfcBt+z8kdB1eYqUYxn80dDgid0Pllp6EWBBZr1mhzUkkKeVWGt0cHnItt9YxiDqHVpD0RzXbLDWTgDNwHjJX (none)"
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
    };

    home.file.".bashrc".text = ''
      . ${esc wenzels-bash.history-settings-file-path}
    '';
  };
}
