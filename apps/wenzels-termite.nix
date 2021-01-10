{ pkgs  ? import <nixpkgs> {}
, utils ? import (import ../nix/sources.nix).nix-utils { inherit pkgs; }
}:
let
  inherit (utils) esc wrapExecutable;

  # TODO Pin using “niv”
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
  default = termite "termite"       darkConfig;
  dark    = termite "termite-dark"  darkConfig;
  light   = termite "termite-light" lightConfig;
}
