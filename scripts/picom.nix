args@{ ... }:
let pkgs-k = "pkgs"; utils-k = "utils"; config-k = "config"; in
assert let k = pkgs-k;  in builtins.hasAttr k args -> builtins.isAttrs args.${k};
assert let k = utils-k; in builtins.hasAttr k args -> builtins.isAttrs args.${k};
let
  pkgs = args.${pkgs-k} or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { ${k} = args.${k}.nixpkgs.${k}; } else {}
  ));

  sources = import ../nix/sources.nix;
  utils = args.${utils-k} or (import sources.nix-utils { inherit pkgs; });
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;

  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  dash = "${pkgs.dash}/bin/dash";
  picom = "${pkgs.picom}/bin/picom";
  pkill = "${pkgs.procps}/bin/pkill";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable dash}
    ${utils.shellCheckers.fileIsExecutable picom}
    ${utils.shellCheckers.fileIsExecutable pkill}
  '';

  killPicom = ''${esc pkill} -x -U "$USER" -- ${esc (baseNameOf picom)} 2>/dev/null'';
  restartFeh = "if [ -f ~/.fehbg ]; then . ~/.fehbg & fi";

  run-picom = writeCheckedExecutable "run-${name}" checkPhase ''
    #! ${dash}
    exec <&-

    if [ -z "$1" ]; then ${killPicom}; fi

    # --blur-kern 7x7box
    # --blur-kern 11x11gaussian
    # blur='--blur-background --blur-background-fixed --blur-kern 7x7box'

    # --active-opacity 0.9

    ${esc picom} \
      --dbus \
      --backend glx \
      -c \
      -o 0.3 \
      -m 0.9 \
      --xinerama-shadow-crop \
      $blur \
      &

    ${restartFeh}
  '';

  no-picom = writeCheckedExecutable "no-${name}" checkPhase ''
    #! ${dash}
    exec <&-

    ${killPicom}
    sleep 1
    ${restartFeh}
  '';
in
{
  inherit name run-picom no-picom checkPhase;
}
