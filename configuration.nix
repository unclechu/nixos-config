{ config ? {}, ... }:
let
  wenzelUserName            = "wenzel";
  rawdevinputGroupName      = "rawdevinput";
  backlightcontrolGroupName = "backlightcontrol";

  keyRepeat = {
    delay    = 170;
    interval = 30;
  };

  xkb = {
    layout  = "us,ru,fi";
    options = "eurosign:e,grp:shifts_toggle";
  };

  stable-nixpkgs   = import ./nixos-stable-pick.nix   { inherit config; };
  unstable-nixpkgs = import ./nixos-unstable-pick.nix { inherit config; };

  pkgs = stable-nixpkgs.pkgs // {
    # In the NixOS 20.03 nixpkgs "rakudo" package is 2017.01 version,
    # very old one, it's not even Raku yet but Perl6
    # (before the language has been renamed).
    # Whilst in unstable nixpkgs it's 2020.05.
    rakudo = unstable-nixpkgs.pkgs.rakudo;
  };

  moduleArgs = { inherit pkgs config; };
  home-manager = import ./home-manager-pick.nix;

  wenzels-bash       =  import apps/wenzels-bash.nix       moduleArgs;
  wenzels-i3         =  import apps/wenzels-i3.nix         moduleArgs;
  wenzels-termite    =  import apps/wenzels-termite.nix    moduleArgs;
  place-cursor-at    = (import apps/place-cursor-at.nix    moduleArgs).pkg;
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

  hsc2hs-pipe = (import scripts/hsc2hs-pipe.nix (moduleArgs // { inherit ghc gcc; })).pkg;
  timer = (import scripts/timer.nix moduleArgs).pkg;
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

  inherit (import ./utils moduleArgs) esc wrapExecutable;
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

    # Open ports in the firewall.
    # firewall.allowedTCPPorts = [ ... ];
    # firewall.allowedUDPPorts = [ ... ];
    # Or disable the firewall altogether.
    # firewall.enable = false;
  };

  i18n.defaultLocale = "en_US.UTF-8";

  console = {
    font = "Lat2-Terminus16";
    keyMap = "us";
  };

  time.timeZone = "Europe/Helsinki";

  environment = {
    variables = {
      EDITOR = "nvim";
      TERMINAL = "termite";

      # XCompose and XIM setup
      XMODIFIERS = "@im=none";
      QT_IM_MODULE = "xim";
      GTK_IM_MODULE = "xim";

      QT_QPA_PLATFORMTHEME = "qt5ct";
      GTK_THEME = "Adwaita:dark";
    };

    shells = [
      pkgs.bash
      pkgs.dash
      pkgs.zsh
      wenzels-bash.pkg
    ];

    systemPackages = [
      # shell stuff
      pkgs.wget pkgs.curl pkgs.jq pkgs.ripgrep pkgs.dash pkgs.newt
      pkgs.killall pkgs.lsof pkgs.inetutils pkgs.acl
      pkgs.inotify-tools
      pkgs.skim pkgs.fzf # fuzzy search
      pkgs.zip

      # nix stuff
      pkgs.nix-index

      # system stuff
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
      ## etc
      pkgs.gnumake

      # file managers
      pkgs.mc
      pkgs.xfce.thunar

      # audio
      pkgs.pavucontrol
      pkgs.audacious
      pkgs.audacity
      pkgs.ardour

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
      pkgs.gnome3.gnome-screenshot
      pkgs.gnome3.gnome-calculator
      pkgs.gnome3.eog
      pkgs.gnome3.gpaste
      pkgs.scrot
      pkgs.shutter
      pkgs.keepassx2
      pkgs.hledger pkgs.hledger-ui pkgs.hledger-web
      pkgs.dfeet

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

    seahorse.enable = true;
    dconf.enable = true;
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
    };

    opengl = rec {
      enable = true;
      driSupport = true;
      driSupport32Bit = true;
      s3tcSupport = true;

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
      shell = wenzels-bash.pkg;

      extraGroups = [
        "users"
        "wheel"
        "networkmanager"
        rawdevinputGroupName
        backlightcontrolGroupName
        "docker"
        "vboxusers"
      ];

      packages = [
        wenzels-bash.pkg
        wenzels-neovim.neovim
        wenzels-neovim.neovim-qt
        wenzels-neovim.scripts.clean-vim.pkg
        wenzels-neovim.scripts.git-grep-nvr.pkg
        wenzels-neovim.scripts.nvimd.pkg
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
    };

    groups = {
      "${wenzelUserName}".gid = 1989;
      "${rawdevinputGroupName}".gid = 500;
      "${backlightcontrolGroupName}".gid = 501;
    };
  };

  security.wrappers =
    let
      rootSuidGroup = source: group: {
        "${source.name}" = {
          source = source;
          permissions = "u+xs,g+x";
          owner = "root";
          group = group;
        };
      };
    in
      rootSuidGroup grant-access-to-input-devices rawdevinputGroupName //
      rootSuidGroup laptop-backlight backlightcontrolGroupName;

  home-manager.users."${wenzelUserName}" = {
    programs.git = {
      enable = true;
      userName = "Viacheslav Lotsmanov";
      userEmail = "lotsmanov89@gmail.com";
    };
  };
}
