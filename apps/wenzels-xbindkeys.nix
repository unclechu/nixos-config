# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ lib
, callPackage
, symlinkJoin
, makeBinaryWrapper
, runCommand
, writeText
, coreutils
# , xkb-switch
, xautolock
, xbindkeys

# Overridable dependencies
, executable-dependencies ? callPackage ../utils/executable-dependencies.nix {}
}:
let
  name = "wenzels-xbindkeys";
  esc = lib.escapeShellArg;

  executablesMap = {
    mkdir = coreutils;
    cp = coreutils;
    chmod = coreutils;
    # xkb-switch = xkb-switch;
    xautolock = xautolock;
    xbindkeys = xbindkeys;
    xbindkeys_show = xbindkeys;
  };

  e = executable-dependencies executablesMap;

  # Previously I was confused with some weird behavior like layouts were rotating not sequentially.
  # But I realized it’s LShift+RShift for rotating forward and RShift+LShift for backward rotation
  # which is a lot more convenient.
  #
  # Old hooks for one-direction rotation:
  #
  #   "${e.b.xkb-switch} -n"
  #     shift + c:50
  #   "${e.b.xkb-switch} -n"
  #     shift + c:62

  xbindkeysrc = writeText "wenezels-xbindkeysrc" ''
    "${e.b.xautolock} -locknow"
      XF86Eject

    # Can't use nix-store path for “locktop” (recursive dependency).
    "locktop"
      shift + XF86Eject
  '';

  shAssertions = {
    readablePredicate = x: '' [[ -f ${esc x} && -r ${esc x} ]] '';
    check = predicate: x: '' if ! (${predicate x}); then (set -x; ${predicate x}) fi '';
  };

  checkPhase = ''
    ${e.checkPhase}
    ${shAssertions.check shAssertions.readablePredicate xbindkeysrc}
  '';

  configArgs = [ "-f" xbindkeysrc ];

  wenzels-xbindkeys = symlinkJoin {
    inherit name;
    nativeBuildInputs = [ makeBinaryWrapper ];
    paths = [ e.executables.xbindkeys ];
    inherit (e) checkPhase;
    postBuild = ''
      wrapProgram "$out"/bin/${esc (baseNameOf e.b.xbindkeys)} \
        --add-flags ${esc (lib.escapeShellArgs configArgs)}
    '';
  };

  wenzels-xbindkeys_show = symlinkJoin {
    name = "${name}_show";
    nativeBuildInputs = [ makeBinaryWrapper ];
    paths = [ e.executables.xbindkeys_show ];
    inherit (e) checkPhase;
    postBuild = ''
      wrapProgram "$out"/bin/${esc (baseNameOf e.b.xbindkeys_show)} \
        --add-flags ${esc (lib.escapeShellArgs configArgs)}
    '';
  };

  eFinal = executable-dependencies (executablesMap // {
    xbindkeys = wenzels-xbindkeys;
    xbindkeys_show = wenzels-xbindkeys_show;
  });
in
runCommand name {} ''
  set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
  ${eFinal.checkPhase}
  ${eFinal.s.mkdir} -p -- "$out/bin"
  ${eFinal.s.cp} -- ${eFinal.s.xbindkeys} ${eFinal.s.xbindkeys_show} "$out/bin/"
  ${eFinal.s.chmod} +x -- \
    "$out"/bin/${esc (baseNameOf eFinal.b.xbindkeys)} \
    "$out"/bin/${esc (baseNameOf eFinal.b.xbindkeys_show)}
'' // {
  xbindkeys = wenzels-xbindkeys;
  xbindkeys_show = wenzels-xbindkeys_show;
}
