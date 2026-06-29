# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
args@{ pkgs, lib, ... }:

let
  sources = import ../../nix/sources.nix;
  apps = (import ../../my-packages.nix args).my-apps;
  rofi-commands = import ../rofi-commands.nix { inherit pkgs lib; };

  executable-dependencies = pkgs.callPackage ../../utils/executable-dependencies.nix {};

  wenzels-i3-status-generator = pkgs.callPackage ./wenzels-i3-status-generator {};

  make-i3-runtime-bar-config = pkgs.callPackage ./make-i3-runtime-bar-config.nix {
    inherit wenzels-i3-status-generator;
  };

  terminalKinds = [ "new" "attach" "nuke" "new-prompt" ];
  terminalFont = "jetbrains";
  terminalTheme = "dark";

  getTerminalName = kind:
    "tmuxed-alacritty-${terminalFont}-font-${terminalTheme}-${kind}";

  terminalExcecutableMap = lib.pipe terminalKinds [
    (map (kind: let name = getTerminalName kind; in {
      inherit name;
      value = apps.allTerminalEmulators.${name};
    }))
    builtins.listToAttrs
  ];

  e = executable-dependencies ({
    wenzels-i3-status-generator = wenzels-i3-status-generator;
    make-i3-runtime-bar-config = make-i3-runtime-bar-config;
  } // terminalExcecutableMap);

  i3-config = pkgs.callPackage ./config.nix {
    inherit wenzels-i3-status-generator;

    autostart-setup = apps.autostart-setup;
    input-setup = apps.input-setup;
    gpaste-gui = apps.gpaste-gui;
    invert-window-colors = apps.invert-window-colors;
    pamng = apps.pamng;
    screen-backlight = apps.screen-backlight;

    terminalNew = e.b.${getTerminalName "new"};
    terminalAttach = e.b.${getTerminalName "attach"};
    terminalNuke = e.b.${getTerminalName "nuke"};
    terminalNewPrompt = e.b.${getTerminalName "new-prompt"};

    runDark = rofi-commands.run.dark;
    runLight = rofi-commands.run.light;
    drunDark = rofi-commands.drun.dark;
    drunLight = rofi-commands.drun.light;
    selectWindowDark = rofi-commands.window.dark;
    selectWindowLight = rofi-commands.window.light;
  };

  wenzels-i3 = pkgs.symlinkJoin {
    name = "wenzels-i3";
    paths = [ pkgs.i3 ];
    buildInputs = [ pkgs.makeWrapper ];
    postBuild = ''
      ${e.checkPhase}
      # Note that `--run` won’t re-run on i3 reload/restart.
      wrapProgram "$out"/bin/${
        lib.escapeShellArg pkgs.i3.meta.mainProgram
      } --run ${lib.escapeShellArg ''(
        PSEUDO_PRIMARY_DISPLAY_I3_CONFIG=$XDG_RUNTIME_DIR/i3-runtime-bar-config.conf
        # If file doesn’t exist create a dummy initial configuration
        # so that the i3bar actually starts and then when the autostart
        # script runs i3bar can be updated on triggered i3 reload.
        if [[ ! -f "$PSEUDO_PRIMARY_DISPLAY_I3_CONFIG" ]]; then
          printf '
            bar {
              status_command ${e.b.wenzels-i3-status-generator}
              position top
              tray_output primary
            }
          ' > "$PSEUDO_PRIMARY_DISPLAY_I3_CONFIG"
        fi
      )''}
    '';
  };
in

{
  services.xserver.windowManager.i3 = {
    enable = true;
    configFile = i3-config;
    package = wenzels-i3;

    extraPackages = [
      e.executables.wenzels-i3-status-generator
      e.executables.make-i3-runtime-bar-config
      pkgs.i3status
      pkgs.i3lock
      pkgs.adwaita-icon-theme
    ];
  };
}
