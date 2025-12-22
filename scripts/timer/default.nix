# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{ lib
, callPackage
, writeTextFile
, symlinkJoin
, makeBinaryWrapper

, rakudo
, dunst
, dbus

# Build options
, __scriptSrc ? ./timer.raku
}:

let
  executables = {
    raku = rakudo;
    dunstify = dunst;
    dunstctl = dunst;
    dbus-monitor = dbus;
  };

  esc = lib.escapeShellArg;
  bin = pkg: exe: "${pkg}/bin/${exe}";
  e = builtins.mapAttrs (n: v: esc (bin v n)) executables;
  executableFileCheck = x: "[[ -f ${x} || -r ${x} || -x ${x} ]]";

  timerScript = writeTextFile rec {
    name = "timer";
    executable = true;
    destination = "/bin/${name}";
    text = "#! ${e.raku}\n${builtins.readFile __scriptSrc}";
    checkPhase = ''(
      set -o nounset
      ${builtins.concatStringsSep "\n" (map (x: ''
        if ! ${executableFileCheck x}; then (set -o xtrace && ${executableFileCheck x}); fi
      '') (builtins.attrValues e))}
    )'';
  };

  wrappedTimerScript = symlinkJoin {
    name = "${lib.getName timerScript}-wrapped";
    nativeBuildInputs = [ makeBinaryWrapper ];
    paths = [ timerScript ];
    postBuild = ''
      wrapProgram "$out"/bin/${esc (lib.getName timerScript)} \
        --prefix PATH : ${esc (lib.makeBinPath (builtins.attrValues executables))}
    '';
  };
in

wrappedTimerScript // { inherit timerScript; }
