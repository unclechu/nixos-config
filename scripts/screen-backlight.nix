let sources = import ../nix/sources.nix; in
{ callPackage
, lib
, bash

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}
, __dzen-box ? callPackage ./dzen-box { inherit __nix-utils; }
}:
assert lib.isDerivation __dzen-box;
let
  inherit (__nix-utils) esc writeCheckedExecutable nameOfModuleFile shellCheckers;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  # Name is executable name and value is a derivation that provides that executable
  dependencies = {
    bash = bash;
    ${__dzen-box.name} = __dzen-box;
  };

  executables = builtins.mapAttrs (n: v: "${dependencies.${n}}/bin/${n}") dependencies;

  checkPhase =
    builtins.concatStringsSep "\n"
      (map shellCheckers.fileIsExecutable (builtins.attrValues executables));
in
writeCheckedExecutable name checkPhase ''
  #! ${executables.bash}
  set -e || exit
  exec <&-

  # Guard dependencies
  >/dev/null type -P laptop-backlight

  if (( $# != 1 )) || ! [[ $1 =~ ^(-|\+)?([0-9]+)%$ ]]; then
    (
      echo
      echo "Incorrect arguments: '$@'"
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

  ${esc executables.${__dzen-box.name}} $(( $val * 100 / $MAX ))% yellow
  laptop-backlight set "$val"
''
