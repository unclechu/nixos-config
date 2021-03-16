let sources = import ../nix/sources.nix; in
{ pkgs
, nix-utils ? pkgs.callPackage sources.nix-utils {}
}:
let
  inherit (nix-utils) nameOfModuleFile wrapExecutable esc;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  # previously i was confused with some weird behavior like layouts were rotating not sequentially.
  # but i realized its LShift+RShift for rotating forward and RShift+LShift for backward rotation
  # which is a lot more convenient.
  #
  # old hooks for one-direction rotation:
  #
  #   "${xkb-switch} -n"
  #     shift + c:50
  #   "${xkb-switch} -n"
  #     shift + c:62
  #
  # xkb-switch = "${pkgs.xkb-switch}/bin/xkb-switch";
  # ${nix-utils.shellCheckers.fileIsExecutable xkb-switch}

  xbindkeysrc = pkgs.writeText "xbindkeysrc" ''
    "${xautolock} -locknow"
      XF86Eject

    # recursive dependency, can't use store path
    "locktop"
      shift + XF86Eject
  '';

  xautolock = "${pkgs.xautolock}/bin/xautolock";
  xbindkeys-origin = "${pkgs.xbindkeys}/bin/xbindkeys";
  xbindkeys_show-origin = "${pkgs.xbindkeys}/bin/xbindkeys_show";

  checkPhase = ''
    ${nix-utils.shellCheckers.fileIsExecutable xautolock}
    ${nix-utils.shellCheckers.fileIsExecutable xbindkeys-origin}
    ${nix-utils.shellCheckers.fileIsExecutable xbindkeys_show-origin}
    ${nix-utils.shellCheckers.fileIsReadable xbindkeysrc}
  '';

  configArgs = [ "-f" xbindkeysrc ];

  xbindkeys = wrapExecutable xbindkeys-origin {
    inherit name checkPhase;
    args = configArgs;
  };

  xbindkeys_show = wrapExecutable xbindkeys_show-origin {
    inherit checkPhase;
    name = "${name}_show";
    args = configArgs;
  };
in
pkgs.runCommand name {} ''
  set -Eeuo pipefail
  mkdir -p -- "$out/bin"

  cp -- \
    ${esc xbindkeys}/bin/${esc xbindkeys.name} \
    ${esc xbindkeys_show}/bin/${esc xbindkeys_show.name} \
    "$out"/bin/

  chmod +x -- "$out"/bin/${esc xbindkeys.name} "$out"/bin/${esc xbindkeys_show.name}
'' // {
  inherit xbindkeys xbindkeys_show;
}
