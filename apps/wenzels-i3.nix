{ pkgs ? import <nixpkgs> {} }:
rec {
  rc = (import ../nix/sources.nix).i3rc;
  configFileSrc = builtins.readFile "${rc}/config";

  configFile =
    { autostart-setup
    , input-setup
    , cursor-to-display
    , gpaste-gui
    , pamng
    , screen-backlight
    }:
    let
      autostart-exe = "${autostart-setup}/bin/${autostart-setup.name}";

      patch = builtins.replaceStrings [
        "autostart.sh"
        "input.sh"
        "${cursor-to-display.name}.pl"
        "${gpaste-gui.name}.pl"
        "${pamng.name}.sh"
        "${screen-backlight.name}.sh"
      ] [
        autostart-exe
        "${input-setup}/bin/${input-setup.name}"
        "${cursor-to-display}/bin/${cursor-to-display.name}"
        "${gpaste-gui}/bin/${gpaste-gui.name}"
        "${pamng}/bin/${pamng.name}"
        "${screen-backlight}/bin/${screen-backlight.name}"
      ];
    in
      pkgs.writeText "wenzels-i3-config-file" ''
        ${patch configFileSrc}
        exec_always ${autostart-exe}
      '';
}
