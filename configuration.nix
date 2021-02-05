args@{ config, pkgs, options, ... }:
let
  inherit (import ./constants.nix)
    wenzelUserName xkb keyRepeat
    rawdevinputGroupName backlightcontrolGroupName jackaudioGroupName audioGroupName;

  sources = import nix/sources.nix;
  utils = import sources.nix-utils { inherit pkgs; };
  inherit (utils) esc;

  moduleArgs =
    let
      withConfigArgs =
        let k = "config"; in if builtins.hasAttr k args then { ${k} = args.${k}; } else {};
    in
      withConfigArgs // { inherit pkgs utils; };

  i3-config = let apps = my-packages.my-apps; in import sources.i3rc rec {
    inherit pkgs;
    autostartScript = let app = apps.autostart-setup; in "${app}/bin/${app.name}";

    scriptsPaths = {
      "autostart.sh"         = autostartScript;
      "input.sh"             = let app = apps.input-setup;       in "${app}/bin/${app.name}";
      "cursor-to-display.pl" = let app = apps.cursor-to-display; in "${app}/bin/${app.name}";
      "gpaste-gui.pl"        = let app = apps.gpaste-gui;        in "${app}/bin/${app.name}";
      "pamng.sh"             = let app = apps.pamng;             in "${app}/bin/${app.name}";
      "screen-backlight.sh"  = let app = apps.screen-backlight;  in "${app}/bin/${app.name}";
    };
  };

  grant-access-to-input-devices = import utils/grant-access-to-input-devices { inherit pkgs; };
  laptop-backlight              = import utils/laptop-backlight              { inherit pkgs; };

  my-packages = import ./my-packages.nix moduleArgs;

  inherit (my-packages.my-apps) wenzels-bash;
in
{
  imports = [
    (import "${sources.home-manager}/nixos")
    my-packages.configuration
    i3-config
    ./boot.nix
    ./network.nix
    ./machine-specific.nix
  ];

  system = {
    stateVersion = "20.09";
    fsPackages = [pkgs.xfsprogs.bin];
  };

  nix.nixPath =
    options.nix.nixPath.default ++
    [ "nixpkgs-overlays=/etc/nixos/overlays-compat/" ];

  nixpkgs.overlays = import ./overlays;

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

    opengl =
      let
        extras = [
          "mesa"
          "vaapiVdpau"
          "vaapiIntel"
          "libvdpau-va-gl"
          "libva"
        ];
      in
        {
          enable = true;
          driSupport = true;
          driSupport32Bit = true;
          extraPackages = builtins.map (name: pkgs.${name}) extras;
          extraPackages32 = builtins.map (name: pkgs.pkgsi686Linux.${name}) extras;
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
      pkgs.ibm-plex
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
