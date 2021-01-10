{ pkgs ? import <nixpkgs> {} }:
rec {
  # TODO Pin using “niv”
  rc = pkgs.fetchFromGitHub {
    owner = "unclechu";
    repo = "i3rc";
    rev = "aacfdc9d0f7875faf37ccb3aeaa99cfafd10189e"; # ref "master", 15 November 2020
    sha256 = "073wc37d7rx44z0jyiwn1dsynafj70rgc6izhbf44a6arf354w4p";
  };

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
