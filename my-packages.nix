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
    inherit (pkgs-unstable) freetube;
  };

  # *** apps ***

  wenzels-bash       = pkgs.callPackage apps/wenzels-bash.nix      {};
  tmux-config        = pkgs.callPackage sources.tmuxrc             {};
  gpaste-gui         = pkgs.callPackage sources.gpaste-gui         {};
  xlib-keys-hack     = pkgs.callPackage sources.xlib-keys-hack     {};
  gnome-screenshot   = pkgs.callPackage apps/gnome-screenshot.nix  {};
  unclechu-i3-status = pkgs.callPackage sources.unclechu-i3-status {};

  vims = import ./vims.nix {
    inherit pkgs lib;
    neovide-pkgs = import <nixos-unstable> {};
    bashEnvFile = "${wenzels-bash.dir}/.bash_aliases";
  };

  terminal-emulators = import ./terminal-emulators.nix { inherit pkgs lib; };

  wenzels-xlib-keys-hack = pkgs.callPackage apps/wenzels-xlib-keys-hack {};
  wenzels-keyboard-script = pkgs.callPackage scripts/wenzels-keyboard {};
  wenzels-xbindkeys = pkgs.callPackage apps/wenzels-xbindkeys.nix {};

  firefox = pkgs.callPackage apps/firefox.nix {};

  nheko = pkgs.callPackage apps/nheko.nix {};

  hell = pkgs.callPackage apps/hell.nix {};

  midi-trigger = pkgs.callPackage sources.MIDI-Trigger {
    src = sources.MIDI-Trigger;
  };

  pspg = pkgs.callPackage apps/pspg.nix {};

  polybar = pkgs.callPackage gui/polybar/polybar.nix {};
  run-polybar = pkgs.callPackage gui/polybar/run-polybar.nix { inherit polybar; };

  # *** scripts ***

  autolock = pkgs.callPackage scripts/autolock.nix {};
  cursor-to-display = pkgs.callPackage "${sources.i3rc}/nix/apps/cursor-to-display.nix" {};

  invert-window-colors = pkgs.callPackage "${sources.i3rc}/nix/apps/invert-window-colors-nim.nix" {
    inherit (pkgs) nim;
  };

  hsc2hs-pipe = wenzels-bash.hsc2hs-pipe;
  timer = wenzels-bash.timer;

  dzen-box = pkgs.callPackage scripts/dzen-box {};
  screen-backlight = pkgs.callPackage scripts/screen-backlight.nix {};
  locktop = pkgs.callPackage scripts/locktop.nix {};
  pamng = pkgs.callPackage scripts/pamng.nix {};
  pa-add-mono-sink = pkgs.callPackage scripts/pa-add-mono-sink.nix {};
  autostart-setup = pkgs.callPackage scripts/autostart-setup.nix { inherit systemConfig; };
  input-setup = pkgs.callPackage scripts/input-setup.nix {};
  picom = pkgs.callPackage scripts/picom.nix {};
  genpass = pkgs.callPackage scripts/genpass.nix {};
  pointers = pkgs.callPackage scripts/pointers.nix {};
  pulseaudio-share-server = pkgs.callPackage scripts/pulseaudio-share-server.nix {};
  rt-audio = pkgs.callPackage scripts/rt-audio {};
  screen-saver = pkgs.callPackage scripts/screen-saver {};

  clunky-toml-json-converter = pkgs.callPackage apps/clunky-toml-json-converter {};
in
{
  my-apps = {
    inherit
      autostart-setup input-setup
      cursor-to-display invert-window-colors gpaste-gui pamng screen-backlight
      ;
    inherit (wenzels-bash) wenzels-bash;
  };

  configuration = {
    imports = [
      vims.configuration
      terminal-emulators.configuration
    ];

    environment.shells = [
      pkgs.bash
      pkgs.dash
      pkgs.zsh
      wenzels-bash.wenzels-bash
    ];

    environment.systemPackages = [
      # shell stuff
      pkgs.wget pkgs.curl pkgs.ripgrep pkgs.dash pkgs.newt
      pkgs.killall pkgs.lsof pkgs.inetutils pkgs.acl
      pkgs.inotify-tools
      pkgs.bindfs
      pkgs.skim pkgs.fzf # fuzzy search
      pkgs.eza
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
      pspg

      # dealing with json/yaml/toml from shell
      pkgs.jq
      pkgs.jo # json creator. see https://github.com/jpmens/jo
      pkgs.gron # json to greppable format converter. see https://github.com/tomnomnom/gron
      pkgs.remarshal # Convert between TOML, YAML and JSON
      clunky-toml-json-converter # Convert between TOML and JSON

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
      pkgs.pulsemixer
      pkgs.jack2
      (pkgs.jack-example-tools.overrideAttrs (old: {
        patches = (old.patches or []) ++ [
          # Big fix for “jack_lsp” “--server” argument buffer overflow.
          # Track the bug fixing progress here:
          # https://github.com/jackaudio/jack-example-tools/issues/88
          (pkgs.fetchpatch {
            name = "jack_lsp-fix-jack-server-argument-buffer-overflow.patch";
            url = "https://github.com/jackaudio/jack-example-tools/pull/89/commits/62aeea4c432c8f91b14888c4dc4c310ef762a865.patch";
            hash = "sha256-TbgJdwsxo9K6wTQ46yHLYDbIJkINNARlb332qC8TWlM=";
          })
        ];
      }))
      pkgs.jack_capture
      pkgs.qjackctl
      pkgs.patchage
      pkgs.patchance
      pkgs.audacious
      pkgs.audacity
      pkgs.ardour_7 # TODO: Try Ardour 8
      pkgs.guitarix
      pkgs.normalize
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
      midi-trigger
      pkgs.CHOWTapeModel
      pkgs.lilv # Provides useful tools like “lv2ls”
      pkgs.jalv

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
      pkgs.graphviz

      # video
      pkgs.vlc
      pkgs.smplayer
      pkgs.mplayer
      (pkgs.wrapMpv (pkgs.mpv-unwrapped.override { jackaudioSupport = true; }) {})
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
      pkgs.d-spy # DBus inspector GUI (a C rewrite of “dfeet”)
      pkgs.obs-studio
      pkgs.kitty # TODO: configure
      pkgs.piper # GUI for “ratbagd” service
      pkgs.libnotify
      polybar
      run-polybar

      # camera
      pkgs.v4l-utils
      pkgs.gnome3.cheese

      # instant messaging/communication
      (pkgs.callPackage apps/psi-plus.nix {})
      pkgs.hexchat
      pkgs.weechat
      nheko
      (pkgs.callPackage apps/dino.nix {})
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
      unstable.freetube

      # task management
      pkgs.hledger pkgs.hledger-ui pkgs.hledger-web
      # FIXME: In the 23.11 nixpkgs it is marked a broken.
      # pkgs.taskell # Kanban board TUI written in Haskell

      # version control
      pkgs.git

      # encryption
      pkgs.gnupg
      pkgs.pass
      pkgs.monkeysphere
      pkgs.keepassxc

      # antivirus
      pkgs.clamav
      pkgs.lynis
      pkgs.vulnix

      # extra utils
      pkgs.mat2
    ];

    programs.tmux = {
      enable = true;
      extraConfig = tmux-config.config;
    };

    # Some plugins for Thunar file manager.
    # Thunar itself is added to “environment.systemPackages”.
    programs.thunar.plugins = [
      pkgs.xfce.thunar-archive-plugin
      pkgs.xfce.thunar-volman
    ];

    services.xserver.windowManager.i3.extraPackages = [
      pkgs.i3status
      pkgs.i3lock
      unclechu-i3-status
      pkgs.gnome3.adwaita-icon-theme
    ];

    users.users.${wenzelUserName}.packages = [
      wenzels-bash.wenzels-bash
      wenzels-xlib-keys-hack
      wenzels-xbindkeys
      wenzels-keyboard-script
      input-setup
      autostart-setup
      autolock
      cursor-to-display
      # FIXME: Fails to compile after migrated to 23.11
      # invert-window-colors
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
      pulseaudio-share-server
      rt-audio
      screen-saver
    ] ++ builtins.filter lib.isDerivation (builtins.attrValues pointers);
  };
}
