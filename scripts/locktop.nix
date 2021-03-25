let sources = import ../nix/sources.nix; in
{ callPackage
, lib
, bash
, xautolock
, i3lock
, jack2

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}
, __dzen-box ? callPackage ./dzen-box { inherit __nix-utils; }
, wenzels-keyboard ? callPackage ./wenzels-keyboard { inherit __nix-utils; }
}:
assert lib.isDerivation __dzen-box;
assert lib.isDerivation wenzels-keyboard;
let
  inherit (__nix-utils) esc writeCheckedExecutable nameOfModuleFile shellCheckers;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  # Name is executable name and value is a derivation that provides that executable
  dependencies = {
    bash = bash;
    xautolock = xautolock;
    i3lock = i3lock;
    jack_control = jack2;

    ${__dzen-box.name} = __dzen-box;
    ${wenzels-keyboard.name} = wenzels-keyboard;
  };

  executables = builtins.mapAttrs (n: v: "${dependencies.${n}}/bin/${n}") dependencies;

  checkPhase =
    builtins.concatStringsSep "\n"
      (map shellCheckers.fileIsExecutable (builtins.attrValues executables));
in
writeCheckedExecutable name checkPhase ''
  #! ${executables.bash}
  ${esc executables.${__dzen-box.name}} LOCK orangered
  ${esc executables.${wenzels-keyboard.name}} --no-xlib-hack
  ${esc executables.jack_control} stop
  if [[ -x ~/.screenlayout/only-laptop.sh ]]; then ~/.screenlayout/only-laptop.sh; fi
  if ! ${esc executables.xautolock} -locknow; then ${esc executables.i3lock} -c 111111; fi
''
