args@{ ... }:
assert let k = "pkgs";  in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
assert let k = "utils"; in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
let
  pkgs = args.pkgs or (import <nixpkgs> {
    config = let k = "config"; in
      if builtins.hasAttr k args then {} else args."${k}".nixpkgs.config;
  });

  xlib-keys-hack = "xlib-keys-hack";
  appArgs = [ xlib-keys-hack ];

  appArgsAssertion =
    let f = a: k: assert builtins.hasAttr k args; assert pkgs.lib.isDerivation args."${k}"; a+1;
    in builtins.foldl' f 0 appArgs;

  appArgExe = k: assert builtins.elem k appArgs; "${builtins.getAttr k args}/bin/${k}";
  appArgDrv = k: assert builtins.elem k appArgs; args."${k}";
in
assert appArgsAssertion == builtins.length appArgs;
let
  utils = args.utils or (import ../../nix-utils-pick.nix args).pkg;
  inherit (utils) esc writeCheckedExecutable nameOfModuleWrapDir;

  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.bash;
  bash = "${pkgs.bash}/bin/bash";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable bash}
    ${
      builtins.concatStringsSep "\n"
        (map (k: utils.shellCheckers.fileIsExecutable (appArgExe k)) appArgs)
    }
  '';

  pkg = writeCheckedExecutable name checkPhase ''
    #! ${bash}
    set -Eeuo pipefail
    exec <&-
    PATH=${esc (appArgDrv xlib-keys-hack)}/bin:$PATH

    # guard dependencies
    >/dev/null which grant-access-to-input-devices
    >/dev/null which xlib-keys-hack
    ${src}
  '';
in
{
  inherit name pkg checkPhase;
}
