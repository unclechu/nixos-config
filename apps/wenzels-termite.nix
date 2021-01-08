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
  inherit (utils) esc wrapExecutable;

  rc = pkgs.fetchFromGitHub {
    owner = "unclechu";
    repo = "termiterc";
    rev = "d127c04277308468828dec17d20f195521dd33ab"; # ref "master", 21 March 2017
    sha256 = "1l71fcjf3dqx0494a3awd63qz9dmim0dzd8f2p91xhhcv4kssm78";
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
