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
    inherit (pkgs-unstable) freetube ardour neovim;
  };

  # *** apps ***

  wenzels-bash       = pkgs.callPackage apps/wenzels-bash.nix      {};
  tmux-config        = pkgs.callPackage sources.tmuxrc             {};
  xlib-keys-hack     = pkgs.callPackage sources.xlib-keys-hack     {};
  gnome-screenshot   = pkgs.callPackage apps/gnome-screenshot.nix  {};
  unclechu-i3-status = pkgs.callPackage sources.unclechu-i3-status {};

  gpaste-gui = pkgs.callPackage sources.gpaste-gui {};

  vims =
    let
      patchedPkgs = pkgs.extend (self: super: {
        # Wanna latest Neovim
        neovim = unstable.neovim;
      });
    in
    import ./vims.nix {
      pkgs = patchedPkgs;
      lib = patchedPkgs.lib;
      bashEnvFile = "${wenzels-bash.dir}/.bash_aliases";
    };

  terminal-emulators = import ./terminal-emulators.nix { inherit pkgs lib; };

  wenzels-xlib-keys-hack = pkgs.callPackage apps/wenzels-xlib-keys-hack {};
  wenzels-keyboard-script = pkgs.callPackage scripts/wenzels-keyboard {};
  wenzels-xbindkeys = pkgs.callPackage apps/wenzels-xbindkeys.nix {};

  firefox = pkgs.callPackage apps/firefox.nix {};

  nheko = pkgs.callPackage apps/nheko {};

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
  wenzels-picom = pkgs.callPackage scripts/wenzels-picom { inherit systemConfig; };
  genpass = pkgs.callPackage scripts/genpass.nix {};
  pointers = pkgs.callPackage scripts/pointers.nix {};
  pulseaudio-share-server = pkgs.callPackage scripts/pulseaudio-share-server.nix {};
  rt-audio = pkgs.callPackage scripts/rt-audio {};
  screen-saver = pkgs.callPackage scripts/screen-saver {};
  render-kicad-schematic-pdf-to-png = pkgs.callPackage scripts/render-kicad-schematic-pdf-to-png {};

  clunky-toml-json-converter = pkgs.callPackage apps/clunky-toml-json-converter {};

  mpv =
    let pkg = pkgs.mpv-unwrapped.override { jackaudioSupport = true; };
    in pkg.wrapper { mpv = pkg; };
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
      tmux-config.tmux-report-current-pane-cwd
      pspg
      # Fix file names extracted from a Windows ZIP archive with cyrillic chars:
      #   unzip archive.zip
      #   convmv --notest -f iso8859-1 -t cp850 *
      #   convmv --notest -f cp866 -t utf8 *
      # TODO: Maybe make a more universal script out of this.
      pkgs.convmv

      # dealing with json/yaml/toml from shell
      pkgs.jq
      pkgs.jo # json creator. see https://github.com/jpmens/jo
      pkgs.gron # json to greppable format converter. see https://github.com/tomnomnom/gron
      pkgs.remarshal # Convert between TOML, YAML and JSON
      clunky-toml-json-converter # Convert between TOML and JSON
      pkgs.pv # monitoring progress of data transfer through a pipeline

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
      pkgs.dmidecode

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
      pkgs.alsa-utils # alsamixer tui
      pkgs.qastools # GUI tools for ALSA configuration
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
      # Use newer Ardour 8.11 instead of 8.8 (8.8 is very buggy)
      unstable.ardour
      pkgs.guitarix
      pkgs.normalize
      pkgs.sonic-visualiser
      pkgs.zita-njbridge # JACK in/out bridge over UDP
      pkgs.espeak
      pkgs.flac # FLAC compression/decompression utility
      pkgs.ffmpeg-normalize # audio normalization utility built on top of FFMPEG
      pkgs.lame # MP3 encoder utility

      # lv2 plugins
      pkgs.calf
      pkgs.artyFX
      pkgs.drumgizmo
      pkgs.eq10q
      # FIXME: Currently broken, see: https://github.com/NixOS/nixpkgs/issues/373829
      # pkgs.fmsynth
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
      pkgs.chow-tape-model
      pkgs.lilv # Provides useful tools like “lv2ls”
      pkgs.jalv
      # Restore old `jalv.gtk` executable by symlinking it to the new `jalv.gtk3`
      (pkgs.symlinkJoin {
        name = "jalv-old-gtk-executable-alias";
        paths = [ pkgs.jalv ];
        postBuild = '' ln -s -- "$out"/bin/jalv.gtk3 "$out"/bin/jalv.gtk '';
      })
      pkgs.lsp-plugins

      # graphics
      pkgs.mesa-demos # e.g. glxinfo for debugging opengl system setup
      pkgs.libva-utils
      pkgs.picom # x11/xorg compositing manager
      pkgs.arandr # simple gui for display/xinerama configuration
      pkgs.autorandr # automatic display configuration based on connected devices
      pkgs.feh # image viewer (for setting desktop background)
      pkgs.gimp # raster graphics editor
      pkgs.inkscape # vector graphics editor
      pkgs.optipng # optimize/compress *.png files
      pkgs.graphviz # draw block-schemes from *.dot files
      pkgs.imagemagick # “convert" for image processing from command-line
      pkgs.ghostscript # converting .pdf into .png using ImageMagick

      # video
      pkgs.vlc
      pkgs.smplayer
      pkgs.mplayer
      mpv
      pkgs.mpvc
      pkgs.ffmpeg-full
      unstable.yt-dlp

      # electronics
      pkgs.kicad # schematics designing tool

      # documents
      pkgs.libreoffice-fresh

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
      pkgs.gnome-system-monitor
      pkgs.gnome-power-manager
      pkgs.gnome-calendar
      pkgs.gnome-calculator
      pkgs.file-roller
      pkgs.eog # Image viewer
      pkgs.evince # Document (e.g. PDF) viwer
      pkgs.d-spy # DBus inspector GUI (a C rewrite of “dfeet”)
      pkgs.obs-studio
      pkgs.kitty # TODO: configure
      pkgs.piper # GUI for “ratbagd” service
      pkgs.libnotify
      polybar
      run-polybar

      # camera
      pkgs.v4l-utils
      pkgs.cheese

      # instant messaging/communication
      (pkgs.callPackage apps/psi-plus.nix {})
      pkgs.hexchat
      pkgs.weechat
      nheko
      pkgs.dino
      pkgs.thunderbird

      # clipboard management
      pkgs.gpaste
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
      pkgs.tcpdump
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
      pkgs.adwaita-icon-theme
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
      wenzels-picom
      pulseaudio-share-server
      rt-audio
      screen-saver
      render-kicad-schematic-pdf-to-png
    ] ++ builtins.filter lib.isDerivation (builtins.attrValues pointers);
  };
}
