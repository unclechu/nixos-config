# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let sources = import ../sources.nix; in

{ lib
, callPackage

, perl
, perlPackages

, xrandr
, xdotool

, executable-dependencies ? callPackage ../../utils/executable-dependencies.nix {}
, mk-generic-script ? callPackage ../../utils/mk-generic-script.nix {}

# Build options
, __srcScript ? ./cursor-to-display.pl
}:

let
  name = "cursor-to-display";

  e = (executable-dependencies {
    perl = perl;
    xrandr = xrandr;
    xdotool = xdotool;
  }).extend (final: prev: {
    scriptDependencies =
      final.dependencies
        "^BEGIN [{] # Guard dependencies$"
        "^[[:space:]]*need_exe '([^']+)';([[:space:]]*#.*)?$";
  });

  perlDependencies = [
    perlPackages.IPCSystemSimple
  ];

  perlDependenciesBinPath = perlPackages.makePerlPath perlDependencies;

  pkg = mk-generic-script rec {
    inherit name e;
    src = __srcScript;
    buildInputs = [ e.executables.perl ];
    lintBuildInputs = [ e.executables.perl ];
    wrapProgramArgs = [ "--set" "PERL5LIB" perlDependenciesBinPath ];

    cutOffRuntimeDependenciesCheckPhase = ''
      SED_CMD=(
        sed -i
        -e '/^BEGIN { # Guard dependencies/,/^}$/d'
        -e '/^use IPC::Cmd qw(can_run)/d'
        -e '/^sub need_exe/d'
        -- "$src"
      )
      "''${SED_CMD[@]}"
    '';

    lintPhase = ''
      (
        export PERL5LIB=${lib.escapeShellArg perlDependenciesBinPath}
        (
          export PATH=${lib.escapeShellArg (e.scriptDependenciesBinPath src)}:$PATH
          perl -c -- "$pre_patched_src"
        )
        perl -c -- "$src"
      )
    '';
  };
in

pkg // { inherit perlDependencies; }
