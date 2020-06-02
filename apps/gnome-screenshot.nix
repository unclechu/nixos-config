args@{ ... }:
assert let k = "pkgs";  in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
assert let k = "utils"; in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
let
  pkgs = args.pkgs or (import <nixpkgs> {
    config = let k = "config"; in
      if builtins.hasAttr k args then {} else args."${k}".nixpkgs.config;
  });

  utils = args.utils or (import ../nix-utils-pick.nix args).pkg;
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  dash = "${pkgs.dash}/bin/dash";
  gnome-screenshot = "${pkgs.gnome3.gnome-screenshot}/bin/${name}";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable dash}
    ${utils.shellCheckers.fileIsExecutable gnome-screenshot}
  '';

  pkg = writeCheckedExecutable name checkPhase ''
    #! ${dash}
    DATE=`date +'%Y-%m-%d %H-%M-%S'` || exit
    ${esc gnome-screenshot} --file="$HOME/Pictures/Screenshots/$DATE.png" "$@"
  '';
in
{
  inherit pkg checkPhase;
}
