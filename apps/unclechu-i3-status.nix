{ pkgs  ? import <nixpkgs> {}
, utils ? import (import ../nix/sources.nix).nix-utils { inherit pkgs; }
}:
let
  inherit (utils) esc wrapExecutable nameOfModuleFile;

  # TODO Pin using “niv”
  src = pkgs.fetchFromGitHub {
    owner = "unclechu";
    repo = "unclechu-i3-status";
    rev = "fe8744725175f5304f9dd70f7aeb91cda1f99a3a"; # ref "master", 22 November 2020
    sha256 = "1qssfqc7sxm12j0m08nbgknxzxs3q68wdyljqjlwyb4bfhpqrcqm";
  };

  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  pkg = pkgs.haskellPackages.callCabal2nix name src {};
  pkg-exe = "${pkgs.haskell.lib.justStaticExecutables pkg}/bin/${name}";
  dzen2 = "${pkgs.dzen2}/bin/dzen2";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable dzen2}
    ${utils.shellCheckers.fileIsExecutable pkg-exe}
  '';
in
wrapExecutable pkg-exe { deps = [ pkgs.dzen2 ]; inherit checkPhase; }
