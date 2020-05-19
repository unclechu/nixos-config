args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, ...
}:
let
  toBashBool = x: if x then "true" else "false";
in
rec {
  esc = pkgs.lib.escapeShellArg;
  lines = str: builtins.filter builtins.isString (builtins.split "\n" str);
  unlines = builtins.concatStringsSep "\n";

  # to get module file path use this hack: (builtins.unsafeGetAttrPos "a" { a = 0; }).file
  nameOfModuleWrapDir = moduleFilePath: baseNameOf (dirOf moduleFilePath);

  # to get module file path use this hack: (builtins.unsafeGetAttrPos "a" { a = 0; }).file
  nameOfModuleFile = moduleFilePath:
    let file = baseNameOf moduleFilePath;
    in  builtins.substring 0 (builtins.stringLength file - (builtins.stringLength ".nix")) file;

  writeCheckedExecutable = name: checkPhase: text: pkgs.writeTextFile {
    inherit name text;
    executable = true;
    destination = "/bin/${name}";
    checkPhase = "set -Eeuo pipefail\n${checkPhase}";
  };

  bash = {
    checkFileIsExecutable = file: ''
      if ! [[ -f ${esc file} && -r ${esc file} && -x ${esc file} ]]; then
        >&2 printf 'File "%s" is supposed to be ' ${esc file}
        >&2 echo 'readable executable file but this assertion has failed!'
        exit 1
      fi
    '';

    checkFileIsReadable = file: ''
      if ! [[ -f ${esc file} && -r ${esc file} ]]; then
        >&2 printf 'File "%s" is supposed to be ' ${esc file}
        >&2 echo 'readable but this assertion has failed!'
        exit 1
      fi
    '';

    checkValueIsPositiveNaturalNumber = value: ''
      if ! ${toBashBool (builtins.isInt value && value >= 1)}; then
        >&2 printf 'Value "%s" is supposed to be ' ${esc value}
        >&2 echo 'a positive natural number but this assertion has failed!'
        exit 1
      fi
    '';

    checkValueIsNonEmptyString = value: ''
      if ! ${toBashBool (builtins.isString value || value != "")}; then
        >&2 printf 'Value "%s" is supposed to be ' ${esc value}
        >&2 echo 'a non empty string but this assertion has failed!'
        exit 1
      fi
    '';
  };
}
