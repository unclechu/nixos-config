let sources = import ../nix/sources.nix; in
{ callPackage
, runCommand
, writeText
# , xkb-switch
, xautolock
, xbindkeys

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}
}:
let
  inherit (__nix-utils) nameOfModuleFile wrapExecutable esc shellCheckers;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  # previously i was confused with some weird behavior like layouts were rotating not sequentially.
  # but i realized its LShift+RShift for rotating forward and RShift+LShift for backward rotation
  # which is a lot more convenient.
  #
  # old hooks for one-direction rotation:
  #
  #   "${xkb-switch-exe} -n"
  #     shift + c:50
  #   "${xkb-switch-exe} -n"
  #     shift + c:62
  #
  # xkb-switch-exe = "${xkb-switch}/bin/xkb-switch";
  # ${shellCheckers.fileIsExecutable xkb-switch-exe}

  xbindkeysrc = writeText "wenezels-xbindkeysrc" ''
    "${xautolock-exe} -locknow"
      XF86Eject

    # recursive dependency, can't use store path
    "locktop"
      shift + XF86Eject
  '';

  xautolock-exe = "${xautolock}/bin/xautolock";
  xbindkeys-origin = "${xbindkeys}/bin/xbindkeys";
  xbindkeys_show-origin = "${xbindkeys}/bin/xbindkeys_show";

  checkPhase = ''
    ${shellCheckers.fileIsExecutable xautolock-exe}
    ${shellCheckers.fileIsExecutable xbindkeys-origin}
    ${shellCheckers.fileIsExecutable xbindkeys_show-origin}
    ${shellCheckers.fileIsReadable xbindkeysrc}
  '';

  configArgs = [ "-f" xbindkeysrc ];

  wenzels-xbindkeys = wrapExecutable xbindkeys-origin {
    inherit name checkPhase;
    args = configArgs;
  };

  wenzels-xbindkeys_show = wrapExecutable xbindkeys_show-origin {
    inherit checkPhase;
    name = "${name}_show";
    args = configArgs;
  };
in
runCommand name {} ''
  set -Eeuo pipefail || exit
  mkdir -p -- "$out/bin"

  cp -- \
    ${esc wenzels-xbindkeys}/bin/${esc wenzels-xbindkeys.name} \
    ${esc wenzels-xbindkeys_show}/bin/${esc wenzels-xbindkeys_show.name} \
    "$out"/bin/

  chmod +x -- "$out"/bin/${esc wenzels-xbindkeys.name} "$out"/bin/${esc wenzels-xbindkeys_show.name}
'' // {
  xbindkeys = wenzels-xbindkeys;
  xbindkeys_show = wenzels-xbindkeys_show;
}
