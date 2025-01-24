# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ callPackage
, dash
, picom
, procps

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}

# Build options
, # System config (e.g. self-reference) to extract machine host name.
  # Set to “null” to use in Nix REPL.
  systemConfig
}:
let
  inherit (__nix-utils) esc writeCheckedExecutable nameOfModuleFile shellCheckers;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  # Name is executable name and value is a derivation that provides that executable
  dependencies = {
    dash = dash;
    picom = picom;
    pkill = procps;
  };

  executables = builtins.mapAttrs (n: v: "${dependencies.${n}}/bin/${n}") dependencies;

  checkPhase =
    builtins.concatStringsSep "\n"
      (map shellCheckers.fileIsExecutable (builtins.attrValues executables));

  killPicom = ''
    ${esc executables.pkill} -x -U "$USER" -- ${esc (baseNameOf executables.picom)} 2>/dev/null
  '';

  restartFeh = "if [ -f ~/.fehbg ]; then . ~/.fehbg & fi";

  hostName = systemConfig.networking.hostName or null;
  rw-wenzel-nixos-laptop = callPackage ../hardware/rw-wenzel-nixos-laptop.nix {};

  optionalVsyncFlag =
    # Enable “vsync” for machines that use “modesetting” driver.
    # After “intel” driver was removed in NixOS 24.11 release “modesetting” is
    # the one that is supposed to be used. But despite
    # “Option "TearFree" "true"” being set there is still tearing everywhere
    # with “modesetting”. So using Picom with “--vsync” is the solution to fix
    # the screen tearing for “modesetting” videodriver.
    if hostName == rw-wenzel-nixos-laptop.networking.hostName
    then "--vsync"
    else "";

  run-picom = writeCheckedExecutable "run-${name}" checkPhase ''
    #! ${executables.dash}
    exec <&-

    if [ -z "$1" ]; then
      ${killPicom}
    fi

    # --blur-kern 7x7box
    # --blur-kern 11x11gaussian
    # blur='--blur-background --blur-background-fixed --blur-kern 7x7box'

    # --active-opacity 0.9

    ${esc executables.picom} \
      --dbus \
      --backend glx \
      -c \
      -o 0.3 \
      -m 0.9 \
      --xinerama-shadow-crop \
      ${optionalVsyncFlag} $blur \
      &

    ${restartFeh}
  '';

  no-picom = writeCheckedExecutable "no-${name}" checkPhase ''
    #! ${executables.dash}
    exec <&-

    ${killPicom}
    sleep 1
    ${restartFeh}
  '';
in
{
  inherit run-picom no-picom;
}
