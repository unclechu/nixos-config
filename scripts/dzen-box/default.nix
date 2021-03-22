let sources = import ../../nix/sources.nix; in
{ callPackage
, bash
, inotify-tools
, gnused
, dzen2

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}
}:
let
  inherit (__nix-utils) esc writeCheckedExecutable nameOfModuleWrapDir shellCheckers;
  name = nameOfModuleWrapDir (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  src = builtins.readFile ./main.bash;
  bash-exe = "${bash}/bin/bash";

  # Name is executable name and value is a derivation that provides that executable
  dependencies = {
    inotifywait = inotify-tools;
    sed = gnused;
    dzen2 = dzen2;
  };

  checkPhase = ''
    ${shellCheckers.fileIsExecutable bash-exe}
    ${
      builtins.concatStringsSep "\n" (
        map
          (k: shellCheckers.fileIsExecutable "${dependencies.${k}}/bin/${k}")
          (builtins.attrNames dependencies)
      )
    }
  '';
in
writeCheckedExecutable name checkPhase ''
  #! ${bash-exe}
  set -e
  exec <&-

  ${
    builtins.concatStringsSep "\n" (
      map
        (drv: "PATH=${esc drv}/bin:$PATH")
        (builtins.attrValues dependencies)
    )
  }

  # Guard dependencies
  ${
    builtins.concatStringsSep "\n" (
      map
        (k: ">/dev/null type -P -- ${esc k}")
        (builtins.attrNames dependencies)
    )
  }

  ${src}
''
