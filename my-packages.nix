# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ pkgs, lib, config, systemConfig ? config, ... }:
let
  sources = import nix/sources.nix;
  inherit (import ./constants.nix) wenzelUserName;
  nix-utils = pkgs.callPackage sources.nix-utils {};
  inherit (nix-utils) wrapExecutable;

  # For Neovim 0.5
  pkgs-unstable = import <nixos-unstable> {};

  system-vim = rec {
    vim = pkgs.vim_configurable.customize {
      name = "vim";
      vimrcConfig.packages.myplugins = {
        start = [pkgs.vimPlugins.vim-nix];
        opt = [];
      };
    };

    neovim = pkgs-unstable.neovim.override {
      configure.packages.myPlugins = {
        start = [pkgs.vimPlugins.vim-nix];
        opt = [];
      };
    };

    neovim-qt = pkgs-unstable.neovim-qt.override { inherit neovim; };
  };

  # *** apps ***

  wenzels-bash       = pkgs.callPackage apps/wenzels-bash.nix      { inherit systemConfig; };
  tmux-config        = pkgs.callPackage sources.tmuxrc             {};
  termite-config     = pkgs.callPackage sources.termiterc          {};
  alacritty-config   = pkgs.callPackage apps/alacritty             {};
  gpaste-gui         = pkgs.callPackage sources.gpaste-gui         {};
  xlib-keys-hack     = pkgs.callPackage sources.xlib-keys-hack     {};
  gnome-screenshot   = pkgs.callPackage apps/gnome-screenshot.nix  {};
  unclechu-i3-status = pkgs.callPackage sources.unclechu-i3-status {};

  bashAliasesFile = "${wenzels-bash.dir}/.bash_aliases";

  wenzels-neovim = pkgs.callPackage apps/wenzels-neovim.nix {
    bashEnvFile = bashAliasesFile;
    neovim      = pkgs-unstable.neovim;
    neovim-qt   = pkgs-unstable.neovim-qt;
  };

  neovim-gtk = pkgs.callPackage apps/neovim-gtk.nix {
    neovim = wenzels-neovim.neovim-for-gui;
    inherit bashAliasesFile;
  };

  wenzels-xlib-keys-hack = pkgs.callPackage apps/wenzels-xlib-keys-hack {};
  wenzels-keyboard-script = pkgs.callPackage scripts/wenzels-keyboard {};
  wenzels-xbindkeys = pkgs.callPackage apps/wenzels-xbindkeys.nix {};

  firefox = wrapExecutable "${pkgs.firefox}/bin/firefox" {
    env = {
      MOZ_USE_XINPUT2 = 1; # support touchscreen scrolling
    };
  };

  # *** scripts ***

  autolock = pkgs.callPackage scripts/autolock.nix {};
  cursor-to-display = pkgs.callPackage "${sources.i3rc}/nix/apps/cursor-to-display.nix" {};

  invert-window-colors =
    pkgs.callPackage "${sources.i3rc}/nix/apps/invert-window-colors-nim.nix" {};

  dzen-box = pkgs.callPackage scripts/dzen-box {};
  hsc2hs-pipe = pkgs.callPackage scripts/hsc2hs-pipe.nix { inherit systemConfig; };
  screen-backlight = pkgs.callPackage scripts/screen-backlight.nix {};
  locktop = pkgs.callPackage scripts/locktop.nix {};
  pamng = pkgs.callPackage scripts/pamng.nix {};
  pa-add-mono-sink = pkgs.callPackage scripts/pa-add-mono-sink.nix {};
  autostart-setup = pkgs.callPackage scripts/autostart-setup.nix { inherit systemConfig; };
  input-setup = pkgs.callPackage scripts/input-setup.nix {};
  picom = pkgs.callPackage scripts/picom.nix {};
  timer = pkgs.callPackage scripts/timer.nix { inherit systemConfig; };
  genpass = pkgs.callPackage scripts/genpass.nix {};

  pointer-dell-latitude-laptop-dot =
    pkgs.callPackage scripts/pointer-dell-latitude-laptop-dot {};

  pointer-dell-latitude-laptop-touchpad =
    pkgs.callPackage scripts/pointer-dell-latitude-laptop-touchpad {};

  pointer-logitech-wireless-ambidextrous-small-mouse =
    pkgs.callPackage scripts/pointer-logitech-wireless-ambidextrous-small-mouse {};

  pointer-logitech-wireless-t650-touchpad =
    pkgs.callPackage scripts/pointer-logitech-wireless-t650-touchpad {};

  pointer-razor-wired-ambidextrous-mouse =
    pkgs.callPackage scripts/pointer-razor-wired-ambidextrous-mouse {};
in
{
  my-apps = {
    inherit
      autostart-setup input-setup
      cursor-to-display invert-window-colors gpaste-gui pamng screen-backlight
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
      pkgs.bindfs
      pkgs.skim pkgs.fzf # fuzzy search
      pkgs.exa
      pkgs.bat
      pkgs.lf
      pkgs.ag
      pkgs.zip pkgs.unzip pkgs.p7zip
      pkgs.parallel
      pkgs.file
      tmux-config.tmuxsh

      # nix stuff
      pkgs.nix-index
      pkgs.niv
      pkgs.direnv

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
      pkgs.normalize
      pkgs.jack_capture

      # lv2 plugins
      pkgs.calf
      pkgs.artyFX
      pkgs.drumgizmo
      pkgs.eq10q
      pkgs.fmsynth
      pkgs.fomp
      pkgs.infamousPlugins
      pkgs.mda_lv2
      pkgs.metersLv2
      pkgs.mod-distortion
      pkgs.noise-repellent
      pkgs.rkrlv2
      pkgs.sorcer
      pkgs.speech-denoiser
      pkgs.swh_lv2
      pkgs.x42-plugins
      pkgs.zam-plugins

      # graphics
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
      pkgs.gmrun pkgs.dmenu pkgs.dzen2
      pkgs.xsel pkgs.xdotool pkgs.numlockx pkgs.xkb-switch
      pkgs.xbindkeys
      xlib-keys-hack
      pkgs.place-cursor-at
      pkgs.xautolock
      pkgs.termite
      pkgs.gnome3.networkmanagerapplet
      pkgs.gnome3.gnome-system-monitor
      pkgs.gnome3.gnome-power-manager
      pkgs.gnome3.gnome-calendar
      pkgs.gnome3.gnome-calculator
      pkgs.gnome3.eog # Image viewer
      pkgs.gnome3.evince # Document (e.g. PDF) viwer
      pkgs.dfeet # DBus inspector GUI
      pkgs.obs-studio

      # instant messaging
      pkgs.psi-plus
      pkgs.hexchat
      pkgs.weechat

      # clipboard management
      pkgs.gnome3.gpaste
      gpaste-gui
      pkgs.clipmenu

      # screenshots
      gnome-screenshot
      pkgs.scrot # An alternative to "gnome-screenshot" (just in case, usually I don't use it)
      pkgs.shutter # Advanced screenshot taking&editing tool with GUI (written in Perl)

      # web browsers
      firefox
      pkgs.chromium

      # task management
      pkgs.hledger pkgs.hledger-ui pkgs.hledger-web
      pkgs.taskell # Kanban board TUI written in Haskell

      # version control
      pkgs.git

      # encryption
      pkgs.gnupg
      pkgs.pass
      pkgs.monkeysphere
      pkgs.keepassx2

      # virtualization & containerization
      pkgs.vagrant

      # antivirus
      pkgs.clamav
      pkgs.lynis
      pkgs.vulnix
    ];

    programs.tmux = {
      enable = true;
      extraConfig = tmux-config.config;
    };

    services.xserver.windowManager.i3.extraPackages = [
      pkgs.i3status
      pkgs.i3lock
      unclechu-i3-status
      pkgs.gnome3.adwaita-icon-theme
    ];

    users.users.${wenzelUserName}.packages = [
      wenzels-bash
      wenzels-neovim.neovim
      wenzels-neovim.neovim-qt
      wenzels-neovim.scripts.clean-vim
      wenzels-neovim.scripts.git-grep-nvr
      wenzels-neovim.scripts.nvimd
      neovim-gtk
      neovim-gtk.g
      wenzels-xlib-keys-hack
      wenzels-xbindkeys
      wenzels-keyboard-script
      pointer-dell-latitude-laptop-dot
      pointer-dell-latitude-laptop-touchpad
      pointer-logitech-wireless-ambidextrous-small-mouse
      pointer-logitech-wireless-t650-touchpad
      pointer-razor-wired-ambidextrous-mouse
      input-setup
      autostart-setup
      autolock
      cursor-to-display
      invert-window-colors
      dzen-box
      locktop
      pamng
      screen-backlight
      hsc2hs-pipe
      timer
      genpass
      pa-add-mono-sink
      picom.run-picom
      picom.no-picom
    ] ++ (
      let extract = lib.attrVals ["default" "dark" "light"]; in
      extract termite-config ++
      extract (termite-config.customize {
        defaultName = "termite-ibm-font";
        font = "IBM Plex Mono";
      })
    ) ++ (
      let extract = lib.attrVals ["default" "dark" "light"]; in
      extract alacritty-config ++
      extract (alacritty-config.customize {
        defaultName = "alacritty-ibm-font";
        font = "IBM Plex Mono";
      })
    );
  };
}
