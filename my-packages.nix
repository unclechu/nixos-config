# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
args@{ pkgs, lib, config, systemConfig ? config, ... }:
let
  sources = import nix/sources.nix;
  inherit (import ./constants.nix) wenzelUserName;
  nix-utils = pkgs.callPackage sources.nix-utils {};
  inherit (nix-utils) wrapExecutable;

  unstable = let
    pkgs-unstable = import <nixos-unstable> {};
  in {
    inherit (pkgs-unstable.python3Packages) yt-dlp;
  };

  system-vim = rec {
    vim = pkgs.vim_configurable.customize {
      name = "vim";
      vimrcConfig.packages.myplugins = {
        start = [pkgs.vimPlugins.vim-nix];
        opt = [];
      };
      vimrcConfig.customRC = ''
        set nocompatible
        set hidden
        syntax on
      '';
    };

    neovim = pkgs.neovim.override {
      configure.packages.myPlugins = {
        start = [pkgs.vimPlugins.vim-nix];
        opt = [];
      };
    };

    neovim-qt = pkgs.neovim-qt.override { inherit neovim; };
    neovide = pkgs.neovide;
  };

  mkCustomFontTerminal = terminalConfig: defaultName: font:
    let extract = lib.attrVals ["default" "dark" "light"]; in
    extract terminalConfig ++
    extract (terminalConfig.customize {
      inherit defaultName font;
    });

  mkCustomFontTerminals = commandNameInfix: font:
    mkCustomFontTerminal termite-config "termite-${commandNameInfix}-font" font
    ++ mkCustomFontTerminal alacritty-config "alacritty-${commandNameInfix}-font" font;

  # *** apps ***

  wenzels-bash       = pkgs.callPackage apps/wenzels-bash.nix      {};
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
    inherit (pkgs) neovim;
  };

  wenzels-xlib-keys-hack = pkgs.callPackage apps/wenzels-xlib-keys-hack {};
  wenzels-keyboard-script = pkgs.callPackage scripts/wenzels-keyboard {};
  wenzels-xbindkeys = pkgs.callPackage apps/wenzels-xbindkeys.nix {};

  firefox = pkgs.callPackage apps/firefox.nix {};

  nheko = pkgs.callPackage apps/nheko.nix {};

  hell = pkgs.callPackage apps/hell.nix {};

  # *** scripts ***

  autolock = pkgs.callPackage scripts/autolock.nix {};
  cursor-to-display = pkgs.callPackage "${sources.i3rc}/nix/apps/cursor-to-display.nix" {};

  invert-window-colors = pkgs.callPackage "${sources.i3rc}/nix/apps/invert-window-colors-nim.nix" {
    inherit (pkgs) nim;
  };

  dzen-box = pkgs.callPackage scripts/dzen-box {};
  hsc2hs-pipe = pkgs.callPackage scripts/hsc2hs-pipe.nix {};
  screen-backlight = pkgs.callPackage scripts/screen-backlight.nix {};
  locktop = pkgs.callPackage scripts/locktop.nix {};
  pamng = pkgs.callPackage scripts/pamng.nix {};
  pa-add-mono-sink = pkgs.callPackage scripts/pa-add-mono-sink.nix {};
  autostart-setup = pkgs.callPackage scripts/autostart-setup.nix { inherit systemConfig; };
  input-setup = pkgs.callPackage scripts/input-setup.nix {};
  picom = pkgs.callPackage scripts/picom.nix {};
  timer = pkgs.callPackage scripts/timer.nix {};
  genpass = pkgs.callPackage scripts/genpass.nix {};
  pointers = pkgs.callPackage scripts/pointers.nix {};
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
      pkgs.tree
      pkgs.bat
      pkgs.lf
      pkgs.silver-searcher
      pkgs.zip pkgs.unzip pkgs.p7zip
      pkgs.parallel
      pkgs.entr
      pkgs.file
      pkgs.socat
      pkgs.pandoc
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
      pkgs.sshfs
      pkgs.curlftpfs
      pkgs.jmtpfs
      pkgs.glib # for “gio”
      pkgs.usbutils
      pkgs.dnsutils
      pkgs.lm_sensors

      # code editing
      system-vim.vim
      system-vim.neovim
      system-vim.neovim-qt
      system-vim.neovide
      pkgs.neovim-remote

      # programming languages
      ## haskell
      pkgs.haskellPackages.ghc
      pkgs.hlint
      (pkgs.haskell.lib.justStaticExecutables pkgs.haskellPackages.hoogle)
      hell
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
      pkgs.sonic-visualiser
      pkgs.zita-njbridge # JACK in/out bridge over UDP
      pkgs.espeak

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
      pkgs.mpvc
      pkgs.ffmpeg-full
      pkgs.youtube-dl
      unstable.yt-dlp

      # desktop environment
      pkgs.tk
      pkgs.xorg.xev
      pkgs.gmrun pkgs.dmenu pkgs.dzen2 pkgs.rofi
      pkgs.xsel pkgs.xdotool pkgs.numlockx pkgs.xkb-switch
      pkgs.xbindkeys
      xlib-keys-hack
      pkgs.place-cursor-at
      pkgs.xautolock
      pkgs.termite
      pkgs.networkmanagerapplet
      pkgs.gnome3.gnome-system-monitor
      pkgs.gnome3.gnome-power-manager
      pkgs.gnome3.gnome-calendar
      pkgs.gnome3.gnome-calculator
      pkgs.gnome3.eog # Image viewer
      pkgs.gnome3.evince # Document (e.g. PDF) viwer
      pkgs.dfeet # DBus inspector GUI
      pkgs.obs-studio
      pkgs.kitty # TODO: configure
      pkgs.piper # GUI for “ratbagd” service
      pkgs.libnotify

      # camera
      pkgs.v4l-utils
      pkgs.gnome3.cheese

      # instant messaging/communication
      (pkgs.callPackage apps/psi-plus.nix {})
      pkgs.hexchat
      pkgs.weechat
      nheko
      pkgs.dino
      pkgs.thunderbird

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
      wenzels-neovim.neovide
      wenzels-neovim.scripts.clean-vim
      wenzels-neovim.scripts.git-grep-nvr
      wenzels-neovim.scripts.nvimd
      wenzels-xlib-keys-hack
      wenzels-xbindkeys
      wenzels-keyboard-script
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
    ] ++ mkCustomFontTerminals "ibm" "IBM Plex Mono"
      ++ mkCustomFontTerminals "iosevka" "IBM Plex Mono"
      ++ mkCustomFontTerminals "jetbrains" "JetBrains Mono"
      ++ builtins.filter lib.isDerivation (builtins.attrValues pointers);
  };
}
