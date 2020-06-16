args@{ ... }:
let pkgs-k = "pkgs"; utils-k = "utils"; config-k = "config"; in
assert let k = pkgs-k;  in builtins.hasAttr k args -> builtins.isAttrs args.${k};
assert let k = utils-k; in builtins.hasAttr k args -> builtins.isAttrs args.${k};
let
  pkgs = args.${pkgs-k} or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { ${k} = args.${k}.nixpkgs.${k}; } else {}
  ));

  wenzels-keyboard = "wenzels-keyboard";
  pointer-dell-latitude-laptop-dot = "pointer-dell-latitude-laptop-dot";
  pointer-dell-latitude-laptop-touchpad = "pointer-dell-latitude-laptop-touchpad";
  pointer-logitech-wireless-ambidextrous-small-mouse =
    "pointer-logitech-wireless-ambidextrous-small-mouse";
  pointer-razor-wired-ambidextrous-mouse = "pointer-razor-wired-ambidextrous-mouse";

  appArgs = [
    wenzels-keyboard
    pointer-dell-latitude-laptop-dot
    pointer-dell-latitude-laptop-touchpad
    pointer-logitech-wireless-ambidextrous-small-mouse
    pointer-razor-wired-ambidextrous-mouse
  ];

  appArgsAssertion =
    let f = a: k: assert builtins.hasAttr k args; assert pkgs.lib.isDerivation args.${k}; a+1;
    in builtins.foldl' f 0 appArgs;

  appArgExe = k: assert builtins.elem k appArgs; "${builtins.getAttr k args}/bin/${k}";
in
assert appArgsAssertion == builtins.length appArgs;
let
  utils = args.${utils-k} or (import ../nix-utils-pick.nix (
    pkgs.lib.filterAttrs (k: _: k == config-k) args // { inherit pkgs; }
  )).pkg;

  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;

  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.bash;

  bash = "${pkgs.bash}/bin/bash";
  xinput = "${pkgs.xlibs.xinput}/bin/xinput";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable bash}
    ${utils.shellCheckers.fileIsExecutable xinput}
    ${
      builtins.concatStringsSep "\n"
        (map (k: utils.shellCheckers.fileIsExecutable (appArgExe k)) appArgs)
    }
  '';

  pkg = writeCheckedExecutable name checkPhase ''
    #! ${bash}
    exec <&- &>/dev/null

    # keyboard
    ${esc (appArgExe wenzels-keyboard)} &

    # mouse
    ${esc (appArgExe pointer-dell-latitude-laptop-dot)} &
    ${esc (appArgExe pointer-dell-latitude-laptop-touchpad)} &
    ${esc (appArgExe pointer-logitech-wireless-ambidextrous-small-mouse)} &
    ${esc (appArgExe pointer-razor-wired-ambidextrous-mouse)} &

    # laptop touchscreen
    ${esc xinput} --map-to-output 'ELAN25B6:00 04F3:0732' eDP1

    exit 0 # prevent returning exit status of the latest command
  '';
in
{
  inherit name pkg checkPhase;
}
