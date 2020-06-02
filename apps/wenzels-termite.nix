args@{ ... }:
assert let k = "pkgs";  in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
assert let k = "utils"; in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
let
  pkgs = args.pkgs or (import <nixpkgs> {
    config = let k = "config"; in
      if builtins.hasAttr k args then {} else args."${k}".nixpkgs.config;
  });

  utils = args.utils or (import ../nix-utils-pick.nix args).pkg;
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
