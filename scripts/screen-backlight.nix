args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, dzen-box
, ...
}:
let
  utils = import ../utils args;
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;

  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  bash = "${pkgs.bash}/bin/bash";
  dzen-box-exe = "${dzen-box}/bin/dzen-box";

  checkPhase = ''
    ${utils.bash.checkFileIsExecutable bash}
    ${utils.bash.checkFileIsExecutable dzen-box-exe}
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

    ${esc dzen-box-exe} $(( $val * 100 / $MAX ))% yellow
    laptop-backlight set "$val"
  '';
in
{
  inherit name pkg checkPhase;
}
