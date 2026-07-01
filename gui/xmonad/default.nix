# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

args@{ pkgs, lib, ... }:

let
  inherit (pkgs.haskell.lib) dontCheck;

  # Extra customizable arguments with default values (below).
  #
  # Can’t embed them into the destructuring of the arguments because it’s a NixOS module,
  # it makes NixOS rebuild fail because of unrecognized attribute.

  # For patching XMonad core itself you can clone https://github.com/xmonad/xmonad repo locally
  # (see “localXmonadPath”) and set this flag to “true”.
  useLocalXmonad = args.useLocalXmonad or false;

  localXmonadPath = args.localXmonadPath or ./xmonad-src;

  wmConfig = builtins.fromTOML (builtins.readFile ../wm-config.toml);

  customArgsMap =
    let
      terminal = wmConfig.terminal-configuration.${wmConfig.terminal}.shell-commands;
      runner = wmConfig.runner-configuration.${wmConfig.runner}.shell-commands;
      musicPlayer = wmConfig.music-player-configuration.${wmConfig.music-player}.shell-commands;

      argsMap = {
        "--xmonadrc-terminal-command-new" = terminal.new;
        "--xmonadrc-terminal-command-attach" = terminal.attach;
        "--xmonadrc-terminal-command-nuke" = terminal.nuke;
        "--xmonadrc-terminal-command-new-prompt" = terminal.new-prompt;

        "--xmonadrc-runner-command-run-cmd" = runner.run-command;
        "--xmonadrc-runner-command-run-app" = runner.run-application;
        "--xmonadrc-runner-command-select-window" = runner.select-window;
        "--xmonadrc-runner-command-select-single-option" = runner.select-single-option;

        "--xmonadrc-music-player-control-command-play" = musicPlayer.play;
        "--xmonadrc-music-player-control-command-play-toggle" = musicPlayer.play-toggle;
        "--xmonadrc-music-player-control-command-prev" = musicPlayer.previous;
        "--xmonadrc-music-player-control-command-next" = musicPlayer.next;
        "--xmonadrc-music-player-control-command-stop" = musicPlayer.stop;
        "--xmonadrc-music-player-control-command-spawn-server" = musicPlayer.spawn-server;
      };

      final = lib.pipe argsMap [
        (builtins.mapAttrs (n:
          # Substitute %TERMINAL_NEW% placeholder
          builtins.replaceStrings
            ["%TERMINAL_NEW%"]
            [(lib.escapeShellArg terminal.new)]
        ))
      ];
    in
      assert builtins.all builtins.isString (builtins.attrValues final);
      final;

  shared-config = builtins.fromTOML (builtins.readFile ./shared-config.toml);
in

{
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = false; # Added in “extraPackages” instead
    config = builtins.readFile ./xmonad.hs;
    ghcArgs = shared-config.ghc-arguments;

    # WARNING! Keep consistent with `gui/xmonad/xmonad-dev.sh` and `gui/xmonad/xmonad.hs`.
    xmonadCliArgs = lib.mapAttrsToList (n: v: "${n}=${v}") customArgsMap;

    extraPackages = hsPkgs:
      let
        # For development & testing
        localXmonad = hsPkgs.callCabal2nix "xmonad" (lib.cleanSource localXmonadPath) {};

        newHsPkgs = if ! useLocalXmonad then hsPkgs else hsPkgs.extend (self: super: {
          # “dontCheck” is just to make XMonad builds faster by skipping testing phase
          xmonad = dontCheck localXmonad;
          xmonad-contrib = dontCheck super.xmonad-contrib;
        });
      in
      [
        newHsPkgs.aeson
        newHsPkgs.qm-interpolated-string
        newHsPkgs.typed-process
        newHsPkgs.hostname
        newHsPkgs.async
        newHsPkgs.xmonad
        newHsPkgs.xmonad-contrib
        newHsPkgs.generic-lens
        newHsPkgs.lens
      ];
  };
}
