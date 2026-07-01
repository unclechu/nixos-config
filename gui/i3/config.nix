# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# i3 configuration file derivation.
#
# These dependencies are left up to you to provide or not in your PATH:
#   - place-cursor-at
#   - gnome-screenshot
#   - gnome-calculator
#   - audacious

let sources = import ../../nix/sources.nix; in

{ lib
, callPackage
, writeTextFile

, i3
, jq
, procps
, xdotool
, gmrun
, dmenu
, rofi
, gnome-screenshot
, shutter

, audacious
, mpvc

, place-cursor-at

, terminal-emulators ? callPackage ../../terminal-emulators.nix {}

, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}

# When actually used in the system i3 config override this (it’s missing systemConfig)!
, autostart-setup ? callPackage ../../scripts/autostart-setup.nix { systemConfig = null; }
, input-setup ? callPackage ../../scripts/input-setup.nix {}
, gpaste-gui ? callPackage sources.gpaste-gui {}
, invert-window-colors ? callPackage ../../apps/invert-window-colors {}
, pamng ? callPackage ../../scripts/pamng {}
, screen-backlight ? callPackage ../../scripts/screen-backlight.nix {}
, wenzels-i3-status-generator ? callPackage ./wenzels-i3-status-generator {}

# ↓ Build options ↓
, __configFile ? ./config
, __wmConfigFile ? ../wm-config.toml
}:

let
  wmConfig = builtins.fromTOML (builtins.readFile __wmConfigFile);

  e = executable-dependencies ({
    i3-nagbar = i3;
    i3-msg = i3;
    i3-sensible-terminal = i3;
    jq = jq;
    pgrep = procps;
    xdotool = xdotool;
    gmrun = gmrun;
    dmenu_run = dmenu;
    rofi = rofi;
    gnome-screenshot = gnome-screenshot;
    shutter = shutter;

    # Music players (for substitution when needed)
    audacious = audacious;
    mpvc = mpvc;

    place-cursor-at = place-cursor-at;

    autostart-setup = autostart-setup;
    input-setup = input-setup;
    gpaste-gui = gpaste-gui;
    invert-window-colors = invert-window-colors;
    pamng = pamng;
    screen-backlight = screen-backlight;
    wenzels-i3-status-generator = wenzels-i3-status-generator;
  } // (
    terminal-emulators.allTerminalEmulators
  ));

  # Check that `b` preserves context of `a` but can be bigger.
  preservesContext =
    let
      fn = a: b:
        assert builtins.isString a && builtins.isString b;
        let bContext = builtins.getContext b; in
        builtins.all
          (k: builtins.hasAttr k bContext)
          (builtins.attrNames (builtins.getContext a));
    in
      assert let a = "${jq}"; b = "${a}"; in fn a b && fn b a;
      assert let a = "${jq}"; b = "${a}${procps}"; in fn a b && !(fn b a);
      fn;

  # Returns `b` of `preservesContext`
  assertPreservesContext = a: b: assert preservesContext a b; b;

  # Replace all executable occurances to full executable paths.
  #
  # See the `assert` test for a usage example.
  #
  # Type: string → { executable-name = executable-bin-path } → string
  replacePathsToExecutables =
    let
      f = executablesBinPaths: input: lib.pipe executablesBinPaths [
        (lib.mapAttrsToList (from: to: { inherit from to; }))
        # When there are partial clashes make sure
        # the longest executables are replaced first.
        # For example `tmuxed-alacritty-…-new` vs. `tmuxed-alacritty-…-new-prompt`
        # (the latter must be replaced first or there will be a broken path).
        (builtins.sort (a: b: builtins.stringLength a.from > builtins.stringLength b.from))
        (lib.foldAttrs (x: a: [x] ++ a) [])
        ({ from, to }: builtins.replaceStrings from to input)
        # Something was actually changed
        (x: assert x != input; x)
        # String context is preserved
        (assertPreservesContext input)
      ];
    in
      assert f { inherit (e.s) i3-nagbar i3-msg; } ''
        foo i3-nagbar bar
        i3-msg two three
      '' == ''
        foo ${e.s.i3-nagbar} bar
        ${e.s.i3-msg} two three
      '';
      f
      ;

  # Replace a string with a given list of substitutions.
  #
  # Type:
  #   [{
  #     context = attrset;
  #     # ↑ Context that will be verified for preservation
  #     match = string → null | [string];
  #     # ↑ Matching function that will be applied against a line
  #     #   (if `null` is returned it’s no match, `sub` is not called)
  #     sub = [string] → string;
  #     # ↑ Result of `match` applied against a line → new line
  #   }]
  #   → string
  #   → string
  replaceWithSubstitutions = substitutions: input:
    assert builtins.isList substitutions;
    assert builtins.all (x:
      builtins.isAttrs x.context &&
      builtins.isFunction x.match &&
      builtins.isFunction x.sub
    ) substitutions;
    assert builtins.isString input;
    lib.pipe input [
      (builtins.split "\n")
      (builtins.filter builtins.isString)
      (map (line:
        builtins.foldl' (acc: x:
          let matchResult = x.match acc; in
          if isNull matchResult then acc else x.sub matchResult
        ) line substitutions
      ))
      (builtins.concatStringsSep "\n")
      (lib.flip builtins.appendContext (builtins.getContext input))
      # The context for substitutions is preserved
      (x:
        let xContext = builtins.getContext x; in
        assert builtins.all (y:
          builtins.all (k: builtins.hasAttr k xContext) (builtins.attrNames y.context)
        ) substitutions; x
      )
      # Something was actually changed
      (x: assert (builtins.length substitutions > 0) -> x != input; x)
      # String context is preserved
      (assertPreservesContext input)
    ];

  # Type: { string = string }
  wmConfigTerminals =
    let cmd = wmConfig.terminal-configuration.${wmConfig.terminal}.shell-commands; in
    assert builtins.isAttrs cmd;
    {
      terminal_new = cmd.new;
      terminal_attach = cmd.attach;
      terminal_nuke = cmd.nuke;
      terminal_new_prompt = cmd.new-prompt;
    };

  # Type: { string = string }
  wmConfigRunners =
    let cmd = wmConfig.runner-configuration.${wmConfig.runner}.shell-commands; in
    assert builtins.isAttrs cmd;
    {
      run_command = cmd.run-command;
      run_application = cmd.run-application;
      select_window = cmd.select-window;
    };

  # Type: { string = string }
  wmConfigMusicPlayer =
    let cmd = wmConfig.music-player-configuration.${wmConfig.music-player}.shell-commands; in
    assert builtins.isAttrs cmd;
    {
      music_player_play = cmd.play;
      music_player_play_toggle = cmd.play-toggle;
      music_player_previous = cmd.previous;
      music_player_next = cmd.next;
      music_player_stop = cmd.stop;
      music_player_spawn_server = cmd.spawn-server;
    };

  # Type: string → string
  wmConfigSubstitutions =
    lib.pipe (
      wmConfigTerminals // wmConfigRunners // wmConfigMusicPlayer
    ) [
      # All those shell commands are supposed to be strings
      (x: assert builtins.all builtins.isString (builtins.attrValues x); x)
      (builtins.mapAttrs (n: substituteTerminalNew))
      (builtins.mapAttrs (n: replacePathsToExecutables e.s))
      (lib.mapAttrsToList (n: v: {
        context = builtins.getContext v;
        match = builtins.match ("^(set \\$" + n + " ).*$");
        sub = x: "${builtins.elemAt x 0}\"${lib.escape ["\""] v}\"";
      }))
      replaceWithSubstitutions
    ];

  # Substitute %TERMINAL_NEW% placeholder
  substituteTerminalNew =
    builtins.replaceStrings
      ["%TERMINAL_NEW%"]
      [(lib.escapeShellArg wmConfigTerminals.terminal_new)];

  patchConfig = lib.flip lib.pipe [
    (replacePathsToExecutables e.s)
    wmConfigSubstitutions
  ];
in

writeTextFile {
  name = "wenzels-i3-config-file";
  inherit (e) checkPhase;
  text = patchConfig (builtins.readFile __configFile);
}
