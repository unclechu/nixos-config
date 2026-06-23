# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../../nix/sources.nix; in

{ lib
, stdenvNoCC
, writeTextFile
, makeBinaryWrapper
, shellcheck
, coreutils
, xrandr
, gawk
, dunst
, procps
, systemd
, dash
}:

let
  # Executable dependencies map.
  # Executable name = Package where the executable comes from.
  executablesMap = {
    xrandr = xrandr;
    dunstctl = dunst;
    pidof = procps;
    awk = gawk;
    rm = coreutils;
    cp = coreutils;
    systemctl = systemd;
    dash = dash;
  };

  # Full paths to the executables.
  # Executable name = Full absolute path to the executable.
  executables =
    builtins.mapAttrs (n: v: lib.getExe' executablesMap.${n} n) executablesMap;

  # `executables` but with shell escaped paths.
  es = builtins.mapAttrs (n: v: lib.escapeShellArg v) executables;

  executablesCheckPhase =
    builtins.concatStringsSep "\n" (
      map (x: ">/dev/null type -- ${x}") (builtins.attrValues es)
    );

  getScriptDependencies =
    lib.flip lib.pipe [
      builtins.readFile
      (lib.splitString "\n")
      (builtins.foldl' (
        acc: line:
          if builtins.isAttrs acc then acc else
          if acc != null then (
            if builtins.isList acc then (
              let match = builtins.match "^>/dev/null type ([^[:space:]]+)$" line; in
              if isNull match then { result = acc; } else acc ++ [(builtins.elemAt match 0)]
            ) else acc
          ) else if line == "# Guard dependencies" then [] else acc
      ) null)
      (x: assert builtins.isAttrs x; x)
      (x: assert builtins.length x.result > 0; x.result)
    ];

  scriptDependenciesToDerivations =
    lib.flip lib.pipe [
      (map (x: executablesMap.${x}))
      lib.unique
    ];

  scriptSrc = ./pseudo-primary-display.sh;

  pseudo-primary-display = stdenvNoCC.mkDerivation rec {
    name = "pseudo-primary-display";
    src = scriptSrc;

    nativeBuildInputs = [
      makeBinaryWrapper
      shellcheck
    ];

    dontUnpack = true;
    doCheck = true;

    checkPhase = ''
      runHook preCheck
      shellcheck -- "$src"
      ${executablesCheckPhase}
      runHook postCheck
    '';

    installPhase = ''
      runHook preInstall

      BIN_PATH="$out/bin/"${lib.escapeShellArg name}
      install -Dm755 -- "$src" "$BIN_PATH"

      CMD=(
        wrapProgram "$BIN_PATH"
        --prefix PATH : ${lib.makeBinPath (lib.pipe src [
          getScriptDependencies
          scriptDependenciesToDerivations
        ])}
      )
      "''${CMD[@]}"

      runHook postInstall
    '';
  };

  getLineByPrefix = prefix:
    lib.pipe scriptSrc [
      builtins.readFile
      (lib.splitString "\n")
      (builtins.filter (line: lib.hasPrefix prefix line))
      # Must find exactly one line
      (x: assert builtins.length x == 1; x)
      builtins.head
    ];

  # Copy selected pseudo primary display number to `$XDG_RUNTIME_DIR` for faster access.
  copyToRuntimeScript = writeTextFile rec {
    name = "copy-pseudo-primary-display-selection-to-runtime";
    executable = true;
    destination = "/bin/${name}";

    text = ''
      #! ${executables.dash}
      set -o errexit || exit; set -o nounset; set -o pipefail
      ${getLineByPrefix "DISPLAY_NUM_FILE="}
      ${getLineByPrefix "DISPLAY_NUM_RUNTIME_FILE="}
      set -o xtrace
      ${es.cp} -vf -- "$DISPLAY_NUM_FILE" "$DISPLAY_NUM_RUNTIME_FILE"
    '';

    checkPhase = ''
      ${executablesCheckPhase}
    '';
  };
in

pseudo-primary-display // { inherit copyToRuntimeScript; }
