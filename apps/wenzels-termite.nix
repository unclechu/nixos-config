let sources = import ../nix/sources.nix; in
{ pkgs
, nix-utils ? pkgs.callPackage sources.nix-utils {}
}:
let
  inherit (nix-utils) esc wrapExecutable;
  inherit (sources) termiterc;
  dash = "${pkgs.dash}/bin/dash";
  termiteBin = "${pkgs.termite}/bin/termite";
  darkConfig = "${termiterc}/config";
  lightConfig = "${termiterc}/config-light";

  checkPhase = ''
    ${nix-utils.shellCheckers.fileIsExecutable dash}
    ${nix-utils.shellCheckers.fileIsExecutable termiteBin}
    ${nix-utils.shellCheckers.fileIsReadable darkConfig}
    ${nix-utils.shellCheckers.fileIsReadable lightConfig}
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
