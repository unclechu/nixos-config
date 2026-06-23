# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let
  constants = import ../../constants.nix;
  sources = import ../../nix/sources.nix;
in
{ callPackage
, lib
, rakudo
, xset
, setxkbmap
, numlockx
, procps

# Overridable dependencies
, __xbindkeys ? callPackage ../../apps/wenzels-xbindkeys.nix {}
, __xlib-keys-hack-starter ? callPackage ../../apps/wenzels-xlib-keys-hack {}
, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../../utils/mk-generic-script.nix {}

# Build options
, __srcScript ? ./main.raku
, keyRepeatOptions ? constants.keyRepeat
, xkbOptions ? constants.xkb
}:

assert lib.isDerivation __xbindkeys;
assert lib.isDerivation __xlib-keys-hack-starter;

let
  keyRepeatDelay    = keyRepeatOptions.delay;
  keyRepeatInterval = keyRepeatOptions.interval;
  xkbLayout         = xkbOptions.layout;
  xkbOptions'       = xkbOptions.options; # "eurosign:e"
in

assert builtins.isInt keyRepeatDelay && keyRepeatDelay >= 1;
assert builtins.isInt keyRepeatInterval && keyRepeatInterval >= 1;
assert builtins.isString xkbLayout && xkbLayout != "";
assert builtins.isString xkbOptions' && xkbOptions' != "";

let
  e = (executable-dependencies {
    raku = rakudo;
    xset = xset;
    setxkbmap = setxkbmap;
    numlockx = numlockx;
    pkill = procps;

    xbindkeys = __xbindkeys;
    wenzels-xlib-keys-hack = __xlib-keys-hack-starter;
  }).extend (final: prev: {
    scriptDependencies = scriptSrc:
      final.dependencies
        "^BEGIN [{] # Guard dependencies$"
        "^[[:space:]]*need-exe '([^']+)';$"
        scriptSrc;
  });
in

mk-generic-script {
  name = "wenzels-keyboard";
  src = __srcScript;
  inherit e;

  buildInputs = [ e.executables.raku ];
  lintBuildInputs = [ e.executables.raku ];

  wrapProgramArgs = [
    "--add-flag" "--key-repeated-delay=${toString keyRepeatDelay}"
    "--add-flag" "--key-repeated-interval=${toString keyRepeatInterval}"
    "--add-flag" "--xkb-layout=${xkbLayout}"
    "--add-flag" "--xkb-options=${xkbOptions'}"
  ];

  cutOffRuntimeDependenciesCheckPhase = ''
    # The dependencies are already checked, no need to do it in runtime.
    sed -i '/BEGIN { # Guard dependencies/,/^}$/d' "$src"
  '';

  lintPhase = ''
    (
      export PATH=${lib.escapeShellArg (e.scriptDependenciesBinPath __srcScript)}:$PATH
      # Will check executable dependencies and Raku code for that check.
      raku -c -- "$pre_patched_src"
    )
    # Final script, check that after patching that it is not broken.
    raku -c -- "$src"
  '';
}
