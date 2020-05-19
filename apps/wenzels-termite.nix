args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, ...
}:
let
  utils = import ../utils args;
  inherit (utils) esc writeCheckedExecutable;

  rc = fetchGit {
    url = "https://github.com/unclechu/termiterc.git";
    rev = "d127c04277308468828dec17d20f195521dd33ab"; # 21 March 2017
    ref = "master";
  };

  dash = "${pkgs.dash}/bin/dash";
  termiteBin = "${pkgs.termite}/bin/termite";
  darkConfig = "${rc}/config";
  lightConfig = "${rc}/config-light";

  checkPhase = ''
    ${utils.bash.checkFileIsExecutable dash}
    ${utils.bash.checkFileIsExecutable termiteBin}
    ${utils.bash.checkFileIsReadable darkConfig}
    ${utils.bash.checkFileIsReadable lightConfig}
  '';

  termite = name: config: writeCheckedExecutable name checkPhase ''
    #! ${dash}
    ${esc termiteBin} --config=${esc config} "$@" || exit $?
  '';
in {
  inherit rc checkPhase;
  default = termite "termite" darkConfig;
  dark = termite "termite-dark" darkConfig;
  light = termite "termite-light" lightConfig;
}
