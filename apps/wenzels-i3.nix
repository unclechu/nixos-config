args@{ ... }:
let pkgs-k = "pkgs"; config-k = "config"; in
assert let k = pkgs-k; in builtins.hasAttr k args -> builtins.isAttrs args.${k};
let
  pkgs = args.${pkgs-k} or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { ${k} = args.${k}.nixpkgs.${k}; } else {}
  ));
in
rec {
  rc = pkgs.fetchFromGitHub {
    owner = "unclechu";
    repo = "i3rc";
    rev = "8ec6a1b1ba23cad1178acc66b375eefc81d97826"; # ref "master", 5 May 2020
    sha256 = "1dn4l8gi06id147bnrnclpfbv87p6p63mj2f4f6zmnlpsclmdk6w";
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
