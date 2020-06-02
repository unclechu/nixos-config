args@{ ... }:
assert let k = "pkgs";  in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
assert let k = "utils"; in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
let
  pkgs = args.pkgs or (import <nixpkgs> {
    config = let k = "config"; in
      if builtins.hasAttr k args then {} else args."${k}".nixpkgs.config;
  });

  dzen-box = "dzen-box";
  appArgs = [ dzen-box ];

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
    set -e
    exec <&-

    # guard dependencies
    >/dev/null which laptop-backlight

    if (( $# != 1 )) || ! [[ $1 =~ ^(-|\+)?([0-9]+)%$ ]]; then
      (
        echo
        echo "Incorrect arguments: '$@'"
        echo
        echo "Usage examples:"
        echo "  $0 50%"
        echo "  $0 +10%"
        echo "  $0 -10%"
        echo
      ) >&2
      exit 1
    fi

    MAX=$(laptop-backlight get-max)
    OPERATOR=''${BASH_REMATCH[1]}
    val=$(( ''${BASH_REMATCH[2]} * $MAX / 100 ))

    if [[ -n $OPERATOR ]]; then
      CUR=$(laptop-backlight get)
      if [[ $OPERATOR == - ]]; then
        val=$(( $CUR - $val ))
      elif [[ $OPERATOR == + ]]; then
        val=$(( $CUR + $val ))
      fi
    fi

    (( $val < 0    )) && val=0
    (( $val > $MAX )) && val=$MAX

    ${esc (appArgExe dzen-box)} $(( $val * 100 / $MAX ))% yellow
    laptop-backlight set "$val"
  '';
in
{
  inherit name pkg checkPhase;
}
