args@{ ... }:
let pkgs-k = "pkgs"; utils-k = "utils"; config-k = "config"; in
assert let k = pkgs-k;  in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
assert let k = utils-k; in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
let
  pkgs = args."${pkgs-k}" or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { "${k}" = args."${k}".nixpkgs."${k}"; } else {}
  ));

  input-setup = "input-setup";
  autolock = "autolock";
  picom = "picom";
  appArgs = [ input-setup autolock picom ];

  appArgsAssertion =
    let f = a: k: assert builtins.hasAttr k args; assert pkgs.lib.isDerivation args."${k}"; a+1;
    in builtins.foldl' f 0 appArgs;

  appArgExe = k:
    assert builtins.elem k appArgs;
    let name = args."${k}".name; in "${builtins.getAttr k args}/bin/${name}";
in
assert appArgsAssertion == builtins.length appArgs;
let
  utils = args."${utils-k}" or (import ../../nix-utils-pick.nix args).pkg;
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;

  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.bash;

  bash = "${pkgs.bash}/bin/bash";
  pactl = "${pkgs.pulseaudio}/bin/pactl";
  gpaste-client = "${pkgs.gnome3.gpaste}/bin/gpaste-client";
  nm-applet = "${pkgs.gnome3.networkmanagerapplet}/bin/nm-applet";
  xsetroot = "${pkgs.xlibs.xsetroot}/bin/xsetroot";

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

    ${esc pactl} stat # starting pulseaudio local user server

    SCREENLAYOUT=~/.screenlayout/default.sh
    if [[ -f $SCREENLAYOUT && -x $SCREENLAYOUT ]]; then "$SCREENLAYOUT"; fi

    ${esc (appArgExe input-setup)}
    ${esc (appArgExe picom)}
    if [[ -f ~/.fehbg ]]; then . ~/.fehbg & fi

    ${esc (appArgExe autolock)}

    ${esc gpaste-client} & # starting local gpaste daemon
    ${esc nm-applet} &

    ${esc xsetroot} -cursor_name left_ptr # default cursor on an empty workspace

    exit 0 # prevent returning exit status of the latest command
  '';
in
{
  inherit name pkg checkPhase;
}
