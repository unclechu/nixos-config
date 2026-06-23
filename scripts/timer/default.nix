# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{ lib
, callPackage

, rakudo
, dunst
, dbus

, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../../utils/mk-generic-script.nix {}

# Build options
, __scriptSrc ? ./timer.raku
}:

let
  e = (executable-dependencies {
    raku = rakudo;
    dunstify = dunst;
    dunstctl = dunst;
    dbus-monitor = dbus;
  }).extend (final: prev: {
    scriptDependencies = scriptSrc:
      final.dependencies
        "^BEGIN [{] # Guard dependencies$"
        "^[[:space:]]*need-exe '([^']+)';$"
        scriptSrc;
  });
in

mk-generic-script {
  name = "timer";
  src = __scriptSrc;
  inherit e;

  buildInputs = [ e.executables.raku ];
  lintBuildInputs = [ e.executables.raku ];

  cutOffRuntimeDependenciesCheckPhase = ''
    # The dependencies are already checked, no need to do it in runtime.
    sed -i '/BEGIN { # Guard dependencies/,/^}$/d' "$src"
  '';

  lintPhase = ''
    (
      export PATH=${lib.escapeShellArg (e.scriptDependenciesBinPath __scriptSrc)}:$PATH
      # Will check executable dependencies and Raku code for that check.
      raku -c -- "$pre_patched_src"
    )
    # Final script, check that after patching that it is not broken.
    raku -c -- "$src"
  '';
}
