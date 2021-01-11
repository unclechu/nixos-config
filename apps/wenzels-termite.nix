let sources = import ../nix/sources.nix; in
{ pkgs  ? import <nixpkgs> {}
, utils ? import sources.nix-utils { inherit pkgs; }
}:
let
  inherit (utils) esc wrapExecutable;
  inherit (sources) termiterc;
  dash = "${pkgs.dash}/bin/dash";
  termiteBin = "${pkgs.termite}/bin/termite";
  darkConfig = "${termiterc}/config";
  lightConfig = "${termiterc}/config-light";

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
