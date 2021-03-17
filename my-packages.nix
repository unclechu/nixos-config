{ pkgs, config, systemConfig ? config, ... }:
let
  sources = import nix/sources.nix;
  inherit (import ./constants.nix) wenzelUserName;
  nix-utils = pkgs.callPackage sources.nix-utils {};
  inherit (nix-utils) wrapExecutable;

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

  wenzels-bash       = pkgs.callPackage apps/wenzels-bash.nix { inherit systemConfig; };
  wenzels-termite    = import apps/wenzels-termite.nix    { inherit pkgs; };
  gpaste-gui         = import apps/gpaste-gui.nix         { inherit pkgs; };
  xlib-keys-hack     = pkgs.callPackage sources.xlib-keys-hack {};
  place-cursor-at    = import sources.place-cursor-at     { inherit pkgs; };
  gnome-screenshot   = import apps/gnome-screenshot.nix   { inherit pkgs; };
  unclechu-i3-status = import apps/unclechu-i3-status.nix { inherit pkgs; };

  bashAliasesFile = "${wenzels-bash.dir}/.bash_aliases";
  wenzels-neovim  = import apps/wenzels-neovim.nix { inherit pkgs; bashEnvFile = bashAliasesFile; };

  neovim-gtk = import apps/neovim-gtk.nix {
    inherit pkgs bashAliasesFile;
    neovim = wenzels-neovim.neovim-for-gui;
  };

  wenzels-xlib-keys-hack = pkgs.callPackage apps/wenzels-xlib-keys-hack {};
  wenzels-keyboard-script = import scripts/wenzels-keyboard { inherit pkgs; };
  wenzels-xbindkeys = import apps/wenzels-xbindkeys.nix { inherit pkgs; };

  firefox = wrapExecutable "${pkgs.firefox}/bin/firefox" {
    env = {
      MOZ_USE_XINPUT2 = 1; # support touchscreen scrolling
    };
  };

  # *** scripts ***

  autolock = import scripts/autolock.nix { inherit pkgs; };
  cursor-to-display = import "${sources.i3rc}/nix/apps/cursor-to-display.nix" { inherit pkgs; };

  invert-window-colors =
    import "${sources.i3rc}/nix/apps/invert-window-colors-nim.nix" { inherit pkgs; };

  dzen-box = import scripts/dzen-box { inherit pkgs; };
  hsc2hs-pipe = pkgs.callPackage scripts/hsc2hs-pipe.nix { inherit systemConfig; };
  screen-backlight = import scripts/screen-backlight.nix { inherit pkgs; };
  locktop = import scripts/locktop.nix { inherit pkgs; };
  pamng = import scripts/pamng.nix { inherit pkgs; };
  pa-add-mono-sink = import scripts/pa-add-mono-sink.nix { inherit pkgs; };
  autostart-setup = import scripts/autostart-setup.nix { inherit pkgs config; };
  input-setup = import scripts/input-setup.nix { inherit pkgs; };
  picom = import scripts/picom.nix { inherit pkgs; };
  timer = pkgs.callPackage scripts/timer.nix { inherit systemConfig; };
  genpass = import scripts/genpass.nix { inherit pkgs; };

  pointer-dell-latitude-laptop-dot =
    import scripts/pointer-dell-latitude-laptop-dot { inherit pkgs; };

  pointer-dell-latitude-laptop-touchpad =
    import scripts/pointer-dell-latitude-laptop-touchpad { inherit pkgs; };

  pointer-logitech-wireless-ambidextrous-small-mouse =
    import scripts/pointer-logitech-wireless-ambidextrous-small-mouse { inherit pkgs; };

  pointer-logitech-wireless-t650-touchpad =
    import scripts/pointer-logitech-wireless-t650-touchpad { inherit pkgs; };

  pointer-razor-wired-ambidextrous-mouse =
    import scripts/pointer-razor-wired-ambidextrous-mouse { inherit pkgs; };
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
      pkgs.bat
      pkgs.lf
      pkgs.ag
      pkgs.zip pkgs.unzip pkgs.p7zip
      pkgs.parallel
      pkgs.file

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
      pkgs.vulnix

      # etc
      pkgs.git
      pkgs.gnupg
      pkgs.monkeysphere
      pkgs.vagrant
    ];

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
      wenzels-termite.default
      wenzels-termite.dark
      wenzels-termite.light
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
