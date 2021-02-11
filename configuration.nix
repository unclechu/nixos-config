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

    upower.enable = true;

    # see also https://nixos.wiki/wiki/JACK
    # jack = {
    #   jackd.enable = true;
    #   # alsa.enable = true; # support ALSA-only programs via ALSA JACK PCM plugin
    #   # loopback.enable = true; # support ALSA-only programms via loopback device (e.g. Steam)
    # };
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
    };

    home.file.".bashrc".text = ''
      . ${esc wenzels-bash.history-settings-file-path}
    '';
  };
}
