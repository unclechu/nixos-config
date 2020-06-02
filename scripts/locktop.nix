args@{ ... }:
assert let k = "pkgs";  in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
assert let k = "utils"; in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
let
  pkgs = args.pkgs or (import <nixpkgs> {
    config = let k = "config"; in
      if builtins.hasAttr k args then {} else args."${k}".nixpkgs.config;
  });

  dzen-box = "dzen-box";
  wenzels-keyboard = "wenzels-keyboard";
  appArgs = [ dzen-box wenzels-keyboard ];

  appArgsAssertion =
    let f = a: k: assert builtins.hasAttr k args; assert pkgs.lib.isDerivation args."${k}"; a+1;
    in builtins.foldl' f 0 appArgs;

  appArgExe = k: assert builtins.elem k appArgs; "${builtins.getAttr k args}/bin/${k}";
in
assert appArgsAssertion == builtins.length appArgs;
let
  utils = args.utils or (import ../nix-utils-pick.nix args).pkg;
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;

  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.bash;

  bash = "${pkgs.bash}/bin/bash";
  xautolock = "${pkgs.xautolock}/bin/xautolock";
  i3lock = "${pkgs.i3lock}/bin/i3lock";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable bash}
    ${utils.shellCheckers.fileIsExecutable xautolock}
    ${
      builtins.concatStringsSep "\n"
        (map (k: utils.shellCheckers.fileIsExecutable (appArgExe k)) appArgs)
    }
  '';

  pkg = writeCheckedExecutable name checkPhase ''
    #! ${bash}
    ${esc (appArgExe dzen-box)} LOCK orangered
    ${esc (appArgExe wenzels-keyboard)} --no-xlib-hack
    if ! ${esc xautolock} -locknow; then ${esc i3lock} -c 111111; fi
  '';
in
{
  inherit name pkg checkPhase;
}
