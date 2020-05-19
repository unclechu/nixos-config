args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, ...
}:
let
  utils = import ../utils args;
  inherit (utils) nameOfModuleFile writeCheckedExecutable esc;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  dash = "${pkgs.dash}/bin/dash";
  xkb-switch = "${pkgs.xkb-switch}/bin/xkb-switch";
  xautolock = "${pkgs.xautolock}/bin/xautolock";

  xbindkeys-origin = "${pkgs.xbindkeys}/bin/xbindkeys";
  xbindkeys_show-origin = "${pkgs.xbindkeys}/bin/xbindkeys_show";

  checkPhase = ''
    ${utils.bash.checkFileIsExecutable dash}
    ${utils.bash.checkFileIsExecutable xkb-switch}
    ${utils.bash.checkFileIsExecutable xautolock}
    ${utils.bash.checkFileIsExecutable xbindkeys-origin}
    ${utils.bash.checkFileIsExecutable xbindkeys_show-origin}
  '';

  xbindkeysrc = pkgs.writeText "xbindkeysrc" ''
    # "${xkb-switch} -n"
    #   shift + c:50
    # "${xkb-switch} -n"
    #   shift + c:62

    "${xautolock} -locknow"
      XF86Eject

    # recursive dependency, can't use store path
    "locktop"
      shift + XF86Eject
  '';

  xbindkeys = ''
    #! ${dash}
    ${esc xbindkeys-origin} -f ${esc xbindkeysrc} "$@"
  '';

  xbindkeys_show = ''
    #! ${dash}
    ${esc xbindkeys_show-origin} -f ${esc xbindkeysrc} "$@"
  '';

  wenzels-xbindkeys = pkgs.runCommand name {} ''
    set -Eeuo pipefail
    mkdir -p -- "$out/bin"
    printf '%s' ${esc xbindkeys} > "$out"/bin/${esc name}
    printf '%s' ${esc xbindkeys_show} > "$out"/bin/${esc name}_show
    chmod +x -- "$out"/bin/${esc name} "$out"/bin/${esc name}_show
  '';
in
{
  inherit name xbindkeys xbindkeys_show xbindkeysrc wenzels-xbindkeys;
}
