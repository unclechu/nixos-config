args@{ ... }:
let config-k = "config"; in
# WARNING! Never assert "config" argument, nowhere in the modules too, it's recursive self-reference.
#          It refers to the configuration itself, to this module we're just about to construct.
#          Otherwise it will fail with infinite recursion error.
# assert let k = config-k; in builtins.hasAttr k args -> builtins.isAttrs args.${k};
let
  wenzelUserName            = "wenzel";
  rawdevinputGroupName      = "rawdevinput";
  backlightcontrolGroupName = "backlightcontrol";
  jackaudioGroupName        = "jackaudio";
  audioGroupName            = "audio";

  keyRepeat = {
    delay    = 170;
    interval = 30;
  };

  xkb = {
    layout  = "us,ru,fi";
    options = "eurosign:e,grp:shifts_toggle";
  };

  withConfigArgs =
    let k = config-k; in if builtins.hasAttr k args then { ${k} = args.${k}; } else {};

  stable-nixpkgs   =  import ./nixos-stable-pick.nix    withConfigArgs;
  unstable-nixpkgs =  import ./nixos-unstable-pick.nix  withConfigArgs;
  utils            = (import ./nix-utils-pick.nix      (withConfigArgs // { inherit pkgs; })).pkg;

  pkgs = stable-nixpkgs.pkgs // rec {
    # In the NixOS 20.03 nixpkgs "rakudo" package is 2017.01 version,
    # very old one, it's not even Raku yet but Perl6
    # (before the language has been renamed).
    # Whilst in unstable nixpkgs it's 2020.05.
    rakudo = unstable-nixpkgs.pkgs.rakudo;

    # Latest on July 28, 2020
    psi-plus = unstable-nixpkgs.pkgs.psi-plus.overrideAttrs (srcAttrs: srcAttrs // rec {
      version = "1.4.1472";

      src = pkgs.fetchFromGitHub {
        owner = "psi-plus";
        repo = "psi-plus-snapshots";
        rev = version;
        sha256 = "1ipgb3m4d5fm21gcyj1k5m7shmdysmdc1dx3gylp2y2ndkp8q8g6";
      };

      cmakeFlags = [
        "-DENABLE_PLUGINS=ON"
        "-DCHAT_TYPE=BASIC"
      ];
    });

    # Released on August 6, 2020
    neovim-unwrapped =
      unstable-nixpkgs.pkgs.neovim-unwrapped.overrideAttrs (srcAttrs: srcAttrs // rec {
        version = "0.4.4";

        src = pkgs.fetchFromGitHub {
          owner = "neovim";
          repo = "neovim";
          rev = "v${version}";
          sha256 = "11zyj6jvkwas3n6w1ckj3pk6jf81z1g7ngg4smmwm7c27y2a6f2m";
        };
      });

    neovim = unstable-nixpkgs.pkgs.wrapNeovim neovim-unwrapped {};
  };

  inherit (utils) esc wrapExecutable;

  moduleArgs = withConfigArgs // { inherit pkgs utils; };
  home-manager = import ./home-manager-pick.nix;

  wenzels-bash       =  import apps/wenzels-bash.nix       moduleArgs;
  wenzels-i3         =  import apps/wenzels-i3.nix         moduleArgs;
  wenzels-termite    =  import apps/wenzels-termite.nix    moduleArgs;
  place-cursor-at    =  import apps/place-cursor-at.nix    moduleArgs;
  unclechu-i3-status = (import apps/unclechu-i3-status.nix moduleArgs).pkg;
  xlib-keys-hack     = (import apps/xlib-keys-hack.nix     moduleArgs).pkg;
  gpaste-gui         = (import apps/gpaste-gui.nix         moduleArgs).pkg;

  wenzels-neovim =
    import apps/wenzels-neovim.nix (moduleArgs // {
      bashEnvFile = "${wenzels-bash.dir}/.bash_aliases";
    });

  wenzels-xlib-keys-hack =
    (import apps/wenzels-xlib-keys-hack (moduleArgs // { inherit xlib-keys-hack; })).pkg;

  wenzels-xbindkeys = (import apps/wenzels-xbindkeys.nix moduleArgs).wenzels-xbindkeys;
  gnome-screenshot = (import apps/gnome-screenshot.nix moduleArgs).pkg;

  grant-access-to-input-devices = (import utils/grant-access-to-input-devices moduleArgs).pkg;
  laptop-backlight              = (import utils/laptop-backlight              moduleArgs).pkg;

  wenzels-keyboard = xlib-keys-hack-starter:
    import scripts/wenzels-keyboard (moduleArgs // {
      inherit keyRepeat xlib-keys-hack-starter;

      # layout rotation is provided by "wenzels-xbindkeys"
      # FIXME this doesn't help to reset it
      xkb = xkb // { options = "eurosign:e"; };

      xbindkeys = wenzels-xbindkeys;
    });

  wenzels-keyboard-script = (wenzels-keyboard wenzels-xlib-keys-hack).pkg;

  pointer-dell-latitude-laptop-dot =
    (import scripts/pointer-dell-latitude-laptop-dot moduleArgs).pkg;

  pointer-dell-latitude-laptop-touchpad =
    (import scripts/pointer-dell-latitude-laptop-touchpad moduleArgs).pkg;

  pointer-logitech-wireless-ambidextrous-small-mouse =
    (import scripts/pointer-logitech-wireless-ambidextrous-small-mouse moduleArgs).pkg;

  pointer-razor-wired-ambidextrous-mouse =
    (import scripts/pointer-razor-wired-ambidextrous-mouse moduleArgs).pkg;

  input-setup = (import scripts/input-setup.nix (moduleArgs // {
    wenzels-keyboard = wenzels-keyboard-script;

    inherit
      pointer-dell-latitude-laptop-dot
      pointer-dell-latitude-laptop-touchpad
      pointer-logitech-wireless-ambidextrous-small-mouse
      pointer-razor-wired-ambidextrous-mouse;
  })).pkg;

  autolock = (import scripts/autolock.nix moduleArgs).pkg;

  autostart-setup = (import scripts/autostart-setup.nix (moduleArgs // {
    inherit input-setup autolock;
    picom = picom.run-picom;
  })).pkg;

  cursor-to-display = (import scripts/cursor-to-display.nix moduleArgs).pkg;
  dzen-box = (import scripts/dzen-box moduleArgs).pkg;

  locktop = (import scripts/locktop.nix (moduleArgs // {
    inherit dzen-box;
    wenzels-keyboard = wenzels-keyboard-script;
  })).pkg;

  pamng = (import scripts/pamng.nix (moduleArgs // { inherit dzen-box; })).pkg;
  pa-add-mono-sink = (import scripts/pa-add-mono-sink.nix moduleArgs).pkg;

  screen-backlight =
    (import scripts/screen-backlight.nix (moduleArgs // { inherit dzen-box; })).pkg;

  hsc2hs-pipe = (import scripts/hsc2hs-pipe.nix (moduleArgs // { inherit ghc gcc; }));
  timer = (import scripts/timer.nix moduleArgs);
  genpass = (import scripts/genpass.nix moduleArgs).pkg;
  picom = import scripts/picom.nix moduleArgs;

  system-vim = rec {
    vim = pkgs.vim_configurable.customize {
      name = "vim";
      vimrcConfig.packages.myplugins = {
        start = [pkgs.vimPlugins.vim-nix];
        opt = [];
      };
    };

    neovim = pkgs.neovim.override {
      configure.packages.myPlugins = {
        start = [pkgs.vimPlugins.vim-nix];
        opt = [];
      };
    };

    neovim-qt = pkgs.neovim-qt.override { inherit neovim; };
  };

  ghc = pkgs.haskellPackages.ghc;
  gcc = pkgs.gcc;

  firefox = wrapExecutable "${pkgs.firefox}/bin/firefox" {
    env = {
      MOZ_USE_XINPUT2 = 1; # support touchscreen scrolling
    };
  };
in
{
  imports = [
    (import "${home-manager}/nixos")
    ./machine-specific.nix
  ];

  system = {
    stateVersion = "20.03";
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

      GTK_THEME = "Adwaita:dark";
    };

    shells = [
      pkgs.bash
      pkgs.dash
      pkgs.zsh
      wenzels-bash
    ];

    systemPackages = [
      # shell stuff
      pkgs.wget pkgs.curl pkgs.jq pkgs.ripgrep pkgs.dash pkgs.newt
      pkgs.killall pkgs.lsof pkgs.inetutils pkgs.acl
      pkgs.inotify-tools
      pkgs.skim pkgs.fzf # fuzzy search
      pkgs.ag
      pkgs.zip pkgs.unzip pkgs.p7zip
      pkgs.parallel-rust

      # nix stuff
      pkgs.nix-index

      # system stuff
      pkgs.htop
      pkgs.dbus pkgs.upower
      pkgs.parted pkgs.gparted
      pkgs.pciutils
      pkgs.wally-cli
      pkgs.sshfs pkgs.curlftpfs

      # code editing
      system-vim.vim
      system-vim.neovim
      system-vim.neovim-qt
      pkgs.neovim-remote

      # programming languages
      ## haskell
      ghc
      pkgs.hlint
      (pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.hoogle)
      ## perls
      pkgs.perl pkgs.rakudo
      ## c
      gcc
      ## bash
      pkgs.shellcheck
      ## etc
      pkgs.gnumake

      # file managers
      pkgs.mc
      pkgs.xfce.thunar

      # audio
      pkgs.pavucontrol
      pkgs.jack2
      pkgs.qjackctl
      pkgs.audacious
      pkgs.audacity
      pkgs.ardour
      unstable-nixpkgs.pkgs.guitarix

      # graphics
      pkgs.libtxc_dxtn_s2tc
      pkgs.glxinfo
      pkgs.libva-utils
      pkgs.picom
      pkgs.arandr
      pkgs.autorandr
      pkgs.feh
      pkgs.gimp
      pkgs.inkscape
      pkgs.optipng

      # video
      pkgs.vlc
      pkgs.smplayer
      pkgs.mplayer
      pkgs.mpv
      pkgs.ffmpeg-full
      pkgs.youtube-dl

      # desktop environment
      pkgs.tk
      pkgs.xlibs.xev
      pkgs.gmrun pkgs.dmenu pkgs.dzen2 pkgs.clipmenu
      pkgs.xsel pkgs.xdotool pkgs.numlockx pkgs.xkb-switch
      pkgs.xbindkeys
      xlib-keys-hack
      place-cursor-at
      pkgs.xautolock
      pkgs.termite
      firefox
      pkgs.chromium
      pkgs.gnome3.networkmanagerapplet
      pkgs.gnome3.gnome-system-monitor
      pkgs.gnome3.gnome-power-manager
      pkgs.gnome3.gnome-calendar
      gnome-screenshot
      pkgs.gnome3.gnome-calculator
      pkgs.gnome3.eog # Image viewer
      pkgs.gnome3.evince # Document (e.g. PDF) viwer
      pkgs.gnome3.gpaste # Clipboard history manager
      pkgs.scrot # An alternative to "gnome-screenshot" (just in case, usually i don't use it)
      pkgs.shutter # Advanced screenshot taking&editing tool with GUI (written in Perl)
      pkgs.keepassx2
      pkgs.hledger pkgs.hledger-ui pkgs.hledger-web
      pkgs.dfeet # DBus inspector GUI
      pkgs.obs-studio
      pkgs.psi-plus

      # antivirus
      pkgs.clamav
      pkgs.lynis
      # pkgs.vulnix # FIXME broken python-ZODB depedency

      # etc
      pkgs.git
      pkgs.gnupg20
      pkgs.monkeysphere
      pkgs.vagrant
    ];

    etc = {
      "xdg/gtk-2.0/gtkrc".text = ''
        gtk-theme-name = Adwaita
      '';
      "xdg/gtk-3.0/settings.ini".text = ''
        [Settings]
        gtk-theme-name = Adwaita
        gtk-application-prefer-dark-theme = true
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
      # s3tcSupport = true; # in nixos-unstable it’s no longer needed

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

        extraPackages = [
          pkgs.i3status
          pkgs.i3lock
          unclechu-i3-status
          pkgs.gnome3.adwaita-icon-theme
        ];

        configFile = wenzels-i3.configFile {
          inherit autostart-setup input-setup cursor-to-display gpaste-gui pamng screen-backlight;
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
    virtualbox.host.enable = true;
  };

  fonts = {
    enableFontDir = true;

    fonts = [
      pkgs.hack-font
      pkgs.fira-code-symbols
      pkgs.iosevka
      pkgs.nerdfonts
      pkgs.terminus_font # for "place-cursor-at"
    ];

    fontconfig = {
      defaultFonts = let font = ["Hack"]; in { monospace = font; sansSerif = font; serif = font; };
      subpixel.rgba = "none";
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

      packages = [
        wenzels-bash
        wenzels-neovim.neovim
        wenzels-neovim.neovim-qt
        wenzels-neovim.scripts.clean-vim
        wenzels-neovim.scripts.git-grep-nvr
        wenzels-neovim.scripts.nvimd
        wenzels-termite.default
        wenzels-termite.dark
        wenzels-termite.light
        wenzels-xlib-keys-hack
        wenzels-xbindkeys
        wenzels-keyboard-script
        pointer-dell-latitude-laptop-dot
        pointer-dell-latitude-laptop-touchpad
        pointer-logitech-wireless-ambidextrous-small-mouse
        pointer-razor-wired-ambidextrous-mouse
        input-setup
        autostart-setup
        autolock
        cursor-to-display
        dzen-box
        locktop
        pamng
        screen-backlight
        gpaste-gui
        hsc2hs-pipe
        timer
        genpass
        pa-add-mono-sink
        picom.run-picom
        picom.no-picom
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
  };
}
