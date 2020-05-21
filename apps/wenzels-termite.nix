args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, ...
}:
let
  utils = import ../utils args;
  inherit (utils) esc wrapExecutable;

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
    ${utils.shellCheckers.fileIsExecutable dash}
    ${utils.shellCheckers.fileIsExecutable termiteBin}
    ${utils.shellCheckers.fileIsReadable darkConfig}
    ${utils.shellCheckers.fileIsReadable lightConfig}
  '';

  termite = name: config: wrapExecutable termiteBin {
    inherit name checkPhase;
    args = [ "--config=${config}" ];
  };
in {
  inherit rc checkPhase;
  default = termite "termite" darkConfig;
  dark = termite "termite-dark" darkConfig;
  light = termite "termite-light" lightConfig;
}
