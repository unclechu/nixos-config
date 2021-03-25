let sources = import ../nix/sources.nix; in
{ callPackage
, runCommand
, writeText
, coreutils
# , xkb-switch
, xautolock
, xbindkeys

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}
}:
let
  inherit (__nix-utils) nameOfModuleFile wrapExecutable esc shellCheckers;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  # Name is executable name and value is a derivation that provides that executable
  dependencies = {
    mkdir = coreutils;
    cp = coreutils;
    chmod = coreutils;
    # xkb-switch = xkb-switch;
    xautolock = xautolock;
    xbindkeys = xbindkeys;
    xbindkeys_show = xbindkeys;
  };

  executables = builtins.mapAttrs (n: v: "${dependencies.${n}}/bin/${n}") dependencies;

  # previously i was confused with some weird behavior like layouts were rotating not sequentially.
  # but i realized its LShift+RShift for rotating forward and RShift+LShift for backward rotation
  # which is a lot more convenient.
  #
  # old hooks for one-direction rotation:
  #
  #   "${executables.xkb-switch} -n"
  #     shift + c:50
  #   "${executables.xkb-switch} -n"
  #     shift + c:62
  #
  # ${shellCheckers.fileIsExecutable executables.xkb-switch}

  xbindkeysrc = writeText "wenezels-xbindkeysrc" ''
    "${executables.xautolock} -locknow"
      XF86Eject

    # recursive dependency, can't use store path
    "locktop"
      shift + XF86Eject
  '';

  checkPhase = ''
    ${
      builtins.concatStringsSep "\n"
        (map shellCheckers.fileIsExecutable (builtins.attrValues executables))
    }
    ${shellCheckers.fileIsReadable xbindkeysrc}
  '';

  configArgs = [ "-f" xbindkeysrc ];

  wenzels-xbindkeys = wrapExecutable executables.xbindkeys {
    inherit name checkPhase;
    args = configArgs;
  };

  wenzels-xbindkeys_show = wrapExecutable executables.xbindkeys_show {
    inherit checkPhase;
    name = "${name}_show";
    args = configArgs;
  };
in
runCommand name {} ''
  set -Eeuo pipefail || exit
  ${esc executables.mkdir} -p -- "$out/bin"

  ${esc executables.cp} -- \
    ${esc wenzels-xbindkeys}/bin/${esc wenzels-xbindkeys.name} \
    ${esc wenzels-xbindkeys_show}/bin/${esc wenzels-xbindkeys_show.name} \
    "$out"/bin/

  ${esc executables.chmod} +x -- \
    "$out"/bin/${esc wenzels-xbindkeys.name} \
    "$out"/bin/${esc wenzels-xbindkeys_show.name}
'' // {
  xbindkeys = wenzels-xbindkeys;
  xbindkeys_show = wenzels-xbindkeys_show;
}
