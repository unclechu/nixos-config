args@{ ... }:
let
  config-k = "config"; pkgs-k = "pkgs"; utils-k = "utils";
  inherit (import ./constants.nix) keyRepeat xkb wenzelUserName;
in
# FIXME asserts cause infinite recursion for some reason
# assert let k = pkgs-k;  in builtins.hasAttr k args -> builtins.isAttrs args.${k};
# assert let k = utils-k; in builtins.hasAttr k args -> builtins.isAttrs args.${k};
let
  withConfigArgs =
    let k = config-k; in if builtins.hasAttr k args then { ${k} = args.${k}; } else {};

  pkgs = args.${pkgs-k} or (import ./pkgs.nix withConfigArgs);

  utils = args.${utils-k} or (
    import ./picks/nix-utils.nix (withConfigArgs // { inherit pkgs; })
  ).pkg;

  inherit (utils) wrapExecutable;
  moduleArgs = withConfigArgs // { inherit pkgs utils; };

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

  # *** apps ***

  wenzels-bash       =  import apps/wenzels-bash.nix       moduleArgs;
  wenzels-termite    =  import apps/wenzels-termite.nix    moduleArgs;
  gpaste-gui         = (import apps/gpaste-gui.nix         moduleArgs).pkg;

  xlib-keys-hack     =  import apps/xlib-keys-hack.nix     moduleArgs;
  place-cursor-at    =  import apps/place-cursor-at.nix    moduleArgs;
  gnome-screenshot   = (import apps/gnome-screenshot.nix   moduleArgs).pkg;
  unclechu-i3-status = (import apps/unclechu-i3-status.nix moduleArgs).pkg;

  wenzels-neovim =
    import apps/wenzels-neovim.nix (moduleArgs // {
      bashEnvFile = "${wenzels-bash.dir}/.bash_aliases";
    });

  wenzels-xlib-keys-hack =
    (import apps/wenzels-xlib-keys-hack (moduleArgs // { inherit xlib-keys-hack; })).pkg;

  wenzels-keyboard = xlib-keys-hack-starter:
    import scripts/wenzels-keyboard (moduleArgs // {
      inherit keyRepeat xlib-keys-hack-starter;

      # layout rotation is provided by "wenzels-xbindkeys"
      # FIXME this doesn't help to reset it
      xkb = xkb // { options = "eurosign:e"; };

      xbindkeys = wenzels-xbindkeys;
    });

  wenzels-keyboard-script = (wenzels-keyboard wenzels-xlib-keys-hack).pkg;
  wenzels-xbindkeys = (import apps/wenzels-xbindkeys.nix moduleArgs).wenzels-xbindkeys;

  firefox = wrapExecutable "${pkgs.firefox}/bin/firefox" {
    env = {
      MOZ_USE_XINPUT2 = 1; # support touchscreen scrolling
    };
  };

  # *** scripts ***

  autolock = (import scripts/autolock.nix moduleArgs).pkg;
  cursor-to-display = (import scripts/cursor-to-display.nix moduleArgs).pkg;
  dzen-box = (import scripts/dzen-box moduleArgs).pkg;

  hsc2hs-pipe = (import scripts/hsc2hs-pipe.nix (moduleArgs // {
    inherit (pkgs.haskellPackages) ghc;
    inherit (pkgs) gcc;
  }));

  screen-backlight =
    (import scripts/screen-backlight.nix (moduleArgs // { inherit dzen-box; })).pkg;

  locktop = (import scripts/locktop.nix (moduleArgs // {
    inherit dzen-box;
    wenzels-keyboard = wenzels-keyboard-script;
  })).pkg;

  pamng = (import scripts/pamng.nix (moduleArgs // { inherit dzen-box; })).pkg;
  pa-add-mono-sink = (import scripts/pa-add-mono-sink.nix moduleArgs).pkg;

  autostart-setup = (import scripts/autostart-setup.nix (moduleArgs // {
    inherit input-setup autolock;
    picom = picom.run-picom;
  })).pkg;

  input-setup = (import scripts/input-setup.nix (moduleArgs // {
    wenzels-keyboard = wenzels-keyboard-script;

    inherit
      pointer-dell-latitude-laptop-dot
      pointer-dell-latitude-laptop-touchpad
      pointer-logitech-wireless-ambidextrous-small-mouse
      pointer-razor-wired-ambidextrous-mouse;
  })).pkg;

  picom = import scripts/picom.nix moduleArgs;
  timer = (import scripts/timer.nix moduleArgs);
  genpass = (import scripts/genpass.nix moduleArgs).pkg;

  pointer-dell-latitude-laptop-dot =
    (import scripts/pointer-dell-latitude-laptop-dot moduleArgs).pkg;

  pointer-dell-latitude-laptop-touchpad =
    (import scripts/pointer-dell-latitude-laptop-touchpad moduleArgs).pkg;

  pointer-logitech-wireless-ambidextrous-small-mouse =
    (import scripts/pointer-logitech-wireless-ambidextrous-small-mouse moduleArgs).pkg;

  pointer-razor-wired-ambidextrous-mouse =
    (import scripts/pointer-razor-wired-ambidextrous-mouse moduleArgs).pkg;
in
{
  my-apps = {
    inherit
      autostart-setup input-setup
      cursor-to-display gpaste-gui pamng screen-backlight
      wenzels-bash;
  };

  configuration = {
    environment.shells = [
      pkgs.bash
      pkgs.dash
      pkgs.zsh
      wenzels-bash
    ];

    environment.systemPackages = [
      # shell stuff
      pkgs.wget pkgs.curl pkgs.jq pkgs.ripgrep pkgs.dash pkgs.newt
      pkgs.killall pkgs.lsof pkgs.inetutils pkgs.acl
      pkgs.inotify-tools
      pkgs.skim pkgs.fzf # fuzzy search
      pkgs.ag
      pkgs.zip pkgs.unzip pkgs.p7zip
      pkgs.parallel-rust
      pkgs.file

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
      pkgs.haskellPackages.ghc
      pkgs.hlint
      (pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.hoogle)
      ## perls
      pkgs.perl pkgs.rakudo
      ## c
      pkgs.gcc
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
      pkgs.guitarix

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

    services.xserver.windowManager.i3.extraPackages = [
      pkgs.i3status
      pkgs.i3lock
      unclechu-i3-status
      pkgs.gnome3.adwaita-icon-theme
    ];

    users.users."${wenzelUserName}".packages = [
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
  };
}
