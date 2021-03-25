let sources = import ../nix/sources.nix; in
{ callPackage
, bash
, xautolock
, i3lock
, procps

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}

# Build options
, minutes ? 5
}:
let
  inherit (__nix-utils) esc writeCheckedExecutable nameOfModuleFile shellCheckers;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  # Name is executable name and value is a derivation that provides that executable
  dependencies = {
    bash = bash;
    xautolock = xautolock;
    i3lock = i3lock;
    pkill = procps;
  };

  executables = builtins.mapAttrs (n: v: "${dependencies.${n}}/bin/${n}") dependencies;

  checkPhase =
    builtins.concatStringsSep "\n"
      (map shellCheckers.fileIsExecutable (builtins.attrValues executables));
in
writeCheckedExecutable name checkPhase ''
  #! ${executables.bash}
  exec <&-

  ${esc executables.pkill} -x -U "$USER" -- ${esc (baseNameOf executables.xautolock)} 2>/dev/null

  ${esc executables.xautolock} \
    -time ${esc minutes} \
    -locker ${esc "${executables.i3lock} -c 111111"} \
    &>/dev/null &
''
