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
, audacious
, shutter

, place-cursor-at

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

, terminalNew       ? null # Optional path to an executable for “new terminal” action
, terminalAttach    ? null # Optional path to an executable for “attach terminal” action
, terminalNuke      ? null # Optional path to an executable for “nuke terminal” action
, terminalNewPrompt ? null # Optional path to an executable for “new prompt terminal” action

, runDark  ? null # Optional path to an executable of command runner (dark  color scheme)
, runLight ? null # Optional path to an executable of command runner (light color scheme)

, drunDark  ? null # Optional path to an executable of desktop application runner (dark  color scheme)
, drunLight ? null # Optional path to an executable of desktop application runner (light color scheme)

, selectWindowDark  ? null # Optional path to an executable of window selection app (dark  color scheme)
, selectWindowLight ? null # Optional path to an executable of window selection app (light color scheme)
}:

let
  e = executable-dependencies {
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
    audacious = audacious;
    shutter = shutter;

    place-cursor-at = place-cursor-at;

    autostart-setup = autostart-setup;
    input-setup = input-setup;
    gpaste-gui = gpaste-gui;
    invert-window-colors = invert-window-colors;
    pamng = pamng;
    screen-backlight = screen-backlight;
    wenzels-i3-status-generator = wenzels-i3-status-generator;
  };

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

  # Type: string → string
  replaceTerminals =
    lib.pipe {
      new = terminalNew;
      attach = terminalAttach;
      nuke = terminalNuke;
      new_prompt = terminalNewPrompt;
    } [
      (lib.filterAttrs (n: v: v != null))
      (lib.mapAttrsToList (n: v: {
        context = builtins.getContext v;
        match = builtins.match "^(set \\$terminal_${n} ).*$";
        sub = x: "${builtins.elemAt x 0}\"${lib.escape ["\""] v}\"";
      }))
      replaceWithSubstitutions
    ];

  # Type: string → string
  replaceRunners =
    lib.pipe {
      run_dark = runDark;
      run_light = runLight;
      drun_dark = drunDark;
      drun_light = drunLight;
    } [
      (lib.filterAttrs (n: v: v != null))
      (lib.mapAttrsToList (n: v: {
        context = builtins.getContext v;
        match = builtins.match ("^(set \\$" + n + " ).*$");
        sub = x: "${builtins.elemAt x 0}\"${lib.escape ["\""] v}\"";
      }))
      replaceWithSubstitutions
    ];

  # Type: string → string
  replaceWindowSelectionApp =
    lib.pipe {
      dark = selectWindowDark;
      light = selectWindowLight;
    } [
      (lib.filterAttrs (n: v: v != null))
      (lib.mapAttrsToList (n: v: {
        context = builtins.getContext v;
        match = builtins.match "^(set \\$select_window_${n} ).*$";
        sub = x: "${builtins.elemAt x 0}\"${lib.escape ["\""] v}\"";
      }))
      replaceWithSubstitutions
    ];

  patchConfig = lib.flip lib.pipe [
    (replacePathsToExecutables e.s)
    replaceTerminals
    replaceRunners
    replaceWindowSelectionApp
  ];
in

writeTextFile {
  name = "wenzels-i3-config-file";
  inherit (e) checkPhase;
  text = patchConfig (builtins.readFile __configFile);
}
