let sources = import ../../nix/sources.nix; in
{ callPackage
, bash
, inotify-tools
, gnused
, dzen2

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}

# Build options
, __srcScript ? ./main.bash
}:
let
  inherit (__nix-utils) esc writeCheckedExecutable nameOfModuleWrapDir shellCheckers;
  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile __srcScript;

  # Name is executable name and value is a derivation that provides that executable
  dependencies = {
    bash = bash;
    inotifywait = inotify-tools;
    sed = gnused;
    dzen2 = dzen2;
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

  ${
    builtins.concatStringsSep "\n" (
      map (drv: "PATH=${esc drv}/bin:$PATH") (builtins.attrValues dependencies)
    )
  }

  # Guard dependencies
  ${
    builtins.concatStringsSep "\n" (
      map (n: ">/dev/null type -P -- ${esc n}") (builtins.attrNames executables)
    )
  }

  ${src}
''
