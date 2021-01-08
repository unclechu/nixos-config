args@{ ... }:
let pkgs-k = "pkgs"; utils-k = "utils"; config-k = "config"; in
assert let k = pkgs-k;  in builtins.hasAttr k args -> builtins.isAttrs args.${k};
assert let k = utils-k; in builtins.hasAttr k args -> builtins.isAttrs args.${k};
let
  pkgs = args.${pkgs-k} or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { ${k} = args.${k}.nixpkgs.${k}; } else {}
  ));

  dzen-box = "dzen-box";
  wenzels-keyboard = "wenzels-keyboard";
  appArgs = [ dzen-box wenzels-keyboard ];

  appArgsAssertion =
    let f = a: k: assert builtins.hasAttr k args; assert pkgs.lib.isDerivation args.${k}; a+1;
    in builtins.foldl' f 0 appArgs;

  appArgExe = k: assert builtins.elem k appArgs; "${builtins.getAttr k args}/bin/${k}";
in
assert appArgsAssertion == builtins.length appArgs;
let
  sources = import ../nix/sources.nix;
  utils = args.${utils-k} or (import sources.nix-utils { inherit pkgs; });
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;

  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.bash;

  bash = "${pkgs.bash}/bin/bash";
  xautolock = "${pkgs.xautolock}/bin/xautolock";
  i3lock = "${pkgs.i3lock}/bin/i3lock";
  jack_control = "${pkgs.jack2}/bin/jack_control";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable bash}
    ${utils.shellCheckers.fileIsExecutable xautolock}
    ${utils.shellCheckers.fileIsExecutable jack_control}
    ${
      builtins.concatStringsSep "\n"
        (map (k: utils.shellCheckers.fileIsExecutable (appArgExe k)) appArgs)
    }
  '';

  pkg = writeCheckedExecutable name checkPhase ''
    #! ${bash}
    ${esc (appArgExe dzen-box)} LOCK orangered
    ${esc (appArgExe wenzels-keyboard)} --no-xlib-hack
    ${esc jack_control} stop
    if [[ -x ~/.screenlayout/only-laptop.sh ]]; then ~/.screenlayout/only-laptop.sh; fi
    if ! ${esc xautolock} -locknow; then ${esc i3lock} -c 111111; fi
  '';
in
{
  inherit name pkg checkPhase;
}
