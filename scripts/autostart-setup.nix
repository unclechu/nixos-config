args@{ ... }:
let pkgs-k = "pkgs"; utils-k = "utils"; config-k = "config"; in
assert let k = pkgs-k;  in builtins.hasAttr k args -> builtins.isAttrs args.${k};
assert let k = utils-k; in builtins.hasAttr k args -> builtins.isAttrs args.${k};
let
  pkgs = args.${pkgs-k} or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { ${k} = args.${k}.nixpkgs.${k}; } else {}
  ));

  input-setup = "input-setup";
  autolock = "autolock";
  picom = "picom";
  appArgs = [ input-setup autolock picom ];

  appArgsAssertion =
    let f = a: k: assert builtins.hasAttr k args; assert pkgs.lib.isDerivation args.${k}; a+1;
    in builtins.foldl' f 0 appArgs;

  appArgExe = k:
    assert builtins.elem k appArgs;
    let name = args.${k}.name; in "${builtins.getAttr k args}/bin/${name}";
in
assert appArgsAssertion == builtins.length appArgs;
let
  sources = import ../nix/sources.nix;
  utils = args.${utils-k} or (import sources.nix-utils { inherit pkgs; });
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;

  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.bash;

  bash = "${pkgs.bash}/bin/bash";
  pactl = "${pkgs.pulseaudio}/bin/pactl";
  gpaste-client = "${pkgs.gnome3.gpaste}/bin/gpaste-client";
  nm-applet = "${pkgs.gnome3.networkmanagerapplet}/bin/nm-applet";
  xsetroot = "${pkgs.xlibs.xsetroot}/bin/xsetroot";

  hostName = args.${config-k}.networking.hostName or null;
  rw-wenzel-nixos-laptop = import ../hardware/rw-wenzel-nixos-laptop.nix args;

  picom-exe =
    if hostName != rw-wenzel-nixos-laptop.networking.hostName
    then esc (appArgExe picom)
    else "";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable bash}
    ${utils.shellCheckers.fileIsExecutable pactl}
    ${utils.shellCheckers.fileIsExecutable gpaste-client}
    ${utils.shellCheckers.fileIsExecutable nm-applet}
    ${utils.shellCheckers.fileIsExecutable xsetroot}
    ${
      builtins.concatStringsSep "\n"
        (map (k: utils.shellCheckers.fileIsExecutable (appArgExe k)) appArgs)
    }
  '';

  pkg = writeCheckedExecutable name checkPhase ''
    #! ${bash}
    exec <&- &>/dev/null

    # audio
    ${esc pactl} stat # starting pulseaudio local user server

    # displays
    SCREENLAYOUT=~/.screenlayout/default.sh
    if [[ -f $SCREENLAYOUT && -x $SCREENLAYOUT ]]; then
      if "$SCREENLAYOUT"; then sleep 1s; fi
    fi
    ${picom-exe}
    if [[ -f ~/.fehbg ]]; then . ~/.fehbg & fi

    ${esc (appArgExe input-setup)}
    ${esc (appArgExe autolock)}
    ${esc gpaste-client} & # starting local gpaste daemon
    ${esc nm-applet} & # starting system tray network manager applet

    ${esc xsetroot} -cursor_name left_ptr # default cursor on an empty workspace

    exit 0 # prevent returning exit status of the latest command
  '';
in
{
  inherit name pkg checkPhase;
}
