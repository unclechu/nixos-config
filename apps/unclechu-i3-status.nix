let sources = import ../nix/sources.nix; in
{ pkgs  ? import <nixpkgs> {}
, utils ? import sources.nix-utils { inherit pkgs; }
}:
let
  inherit (utils) esc wrapExecutable nameOfModuleFile;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  pkg = pkgs.haskellPackages.callCabal2nix name sources.unclechu-i3-status {};
  pkg-exe = "${pkgs.haskell.lib.justStaticExecutables pkg}/bin/${name}";
  dzen2 = "${pkgs.dzen2}/bin/dzen2";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable dzen2}
    ${utils.shellCheckers.fileIsExecutable pkg-exe}
  '';
in
wrapExecutable pkg-exe { deps = [ pkgs.dzen2 ]; inherit checkPhase; }
