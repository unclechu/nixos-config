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

, terminalDark  ? null # Optional path to an executable of terminal emulator (dark  color scheme)
, terminalLight ? null # Optional path to an executable of terminal emulator (light color scheme)

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

  # Type: string → string
  replaceTerminal = input:
    assert builtins.isString input;
    assert terminalDark != null -> builtins.isString terminalDark;
    assert terminalLight != null -> builtins.isString terminalLight;
    let
      matchDark = builtins.match "^(set \\$terminal_dark ).*$";
      matchLight = builtins.match "^(set \\$terminal_light ).*$";
    in
    lib.pipe input [
      (builtins.split "\n")
      (builtins.filter builtins.isString)
      (map (line:
        let
          darkMatch = matchDark line;
          lightMatch = matchLight line;
        in
        if terminalDark != null && darkMatch != null
        then "${builtins.elemAt darkMatch 0}\"${lib.escape ["\""] terminalDark}\""
        else
        if terminalLight != null && lightMatch != null
        then "${builtins.elemAt lightMatch 0}\"${lib.escape ["\""] terminalLight}\""
        else
        line
      ))
      (builtins.concatStringsSep "\n")
      (lib.flip builtins.appendContext (builtins.getContext input))
      # The context for substitutions is preserved
      (x: assert terminalDark != null -> preservesContext terminalDark x; x)
      (x: assert terminalLight != null -> preservesContext terminalLight x; x)
      # Something was actually changed
      (x: assert (terminalDark != null || terminalLight != null) -> x != input; x)
      # String context is preserved
      (assertPreservesContext input)
    ];

  # Type: string → string
  replaceRunners = input:
    assert builtins.isString input;
    assert runDark != null -> builtins.isString runDark;
    assert runLight != null -> builtins.isString runLight;
    assert drunDark != null -> builtins.isString drunDark;
    assert drunLight != null -> builtins.isString drunLight;
    let
      matchRunDark = builtins.match "^(set \\$run_dark ).*$";
      matchRunLight = builtins.match "^(set \\$run_light ).*$";
      matchDrunDark = builtins.match "^(set \\$drun_dark ).*$";
      matchDrunLight = builtins.match "^(set \\$drun_light ).*$";
    in
    lib.pipe input [
      (builtins.split "\n")
      (builtins.filter builtins.isString)
      (map (line:
        let
          runDarkMatch = matchRunDark line;
          runLightMatch = matchRunLight line;
          drunDarkMatch = matchDrunDark line;
          drunLightMatch = matchDrunLight line;
        in
        if runDark != null && runDarkMatch != null
        then "${builtins.elemAt runDarkMatch 0}\"${lib.escape ["\""] runDark}\""
        else
        if runLight != null && runLightMatch != null
        then "${builtins.elemAt runLightMatch 0}\"${lib.escape ["\""] runLight}\""
        else
        if drunDark != null && drunDarkMatch != null
        then "${builtins.elemAt drunDarkMatch 0}\"${lib.escape ["\""] drunDark}\""
        else
        if drunLight != null && drunLightMatch != null
        then "${builtins.elemAt drunLightMatch 0}\"${lib.escape ["\""] drunLight}\""
        else
        line
      ))
      (builtins.concatStringsSep "\n")
      (lib.flip builtins.appendContext (builtins.getContext input))
      # The context for substitutions is preserved
      (x: assert runDark != null -> preservesContext runDark x; x)
      (x: assert runLight != null -> preservesContext runLight x; x)
      (x: assert drunDark != null -> preservesContext drunDark x; x)
      (x: assert drunLight != null -> preservesContext drunLight x; x)
      # Something was actually changed
      (x: assert (
        runDark != null || runLight != null || drunDark != null || drunLight != null
      ) -> x != input; x)
      # String context is preserved
      (assertPreservesContext input)
    ];

  # Type: string → string
  replaceWindowSelectionApp = input:
    assert builtins.isString input;
    assert selectWindowDark != null -> builtins.isString selectWindowDark;
    assert selectWindowLight != null -> builtins.isString selectWindowLight;
    let
      matchSelectWindowDark = builtins.match "^(set \\$select_window_dark ).*$";
      matchSelectWindowLight = builtins.match "^(set \\$select_window_light ).*$";
    in
    lib.pipe input [
      (builtins.split "\n")
      (builtins.filter builtins.isString)
      (map (line:
        let
          selectWindowDarkMatch = matchSelectWindowDark line;
          selectWindowLightMatch = matchSelectWindowLight line;
        in
        if selectWindowDark != null && selectWindowDarkMatch != null
        then "${builtins.elemAt selectWindowDarkMatch 0}\"${lib.escape ["\""] selectWindowDark}\""
        else
        if selectWindowLight != null && selectWindowLightMatch != null
        then "${builtins.elemAt selectWindowLightMatch 0}\"${lib.escape ["\""] selectWindowLight}\""
        else
        line
      ))
      (builtins.concatStringsSep "\n")
      (lib.flip builtins.appendContext (builtins.getContext input))
      # The context for substitutions is preserved
      (x: assert selectWindowDark != null -> preservesContext selectWindowDark x; x)
      (x: assert selectWindowLight != null -> preservesContext selectWindowLight x; x)
      # Something was actually changed
      (x: assert (selectWindowDark != null || selectWindowLight != null) -> x != input; x)
      # String context is preserved
      (assertPreservesContext input)
    ];

  patchConfig = lib.flip lib.pipe [
    (replacePathsToExecutables e.s)
    replaceTerminal
    replaceRunners
    replaceWindowSelectionApp
  ];
in

writeTextFile {
  name = "wenzels-i3-config-file";
  inherit (e) checkPhase;
  text = patchConfig (builtins.readFile __configFile);
}
