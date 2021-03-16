let sources = import ../nix/sources.nix; in
{ pkgs
, nix-utils ? pkgs.callPackage sources.nix-utils {}
}:
let
  inherit (nix-utils) esc wrapExecutable nameOfModuleFile;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  pkg = pkgs.haskellPackages.callCabal2nix name sources.unclechu-i3-status {};
  pkg-exe = "${pkgs.haskell.lib.justStaticExecutables pkg}/bin/${name}";
  dzen2 = "${pkgs.dzen2}/bin/dzen2";

  checkPhase = ''
    ${nix-utils.shellCheckers.fileIsExecutable dzen2}
    ${nix-utils.shellCheckers.fileIsExecutable pkg-exe}
  '';
in
wrapExecutable pkg-exe { deps = [ pkgs.dzen2 ]; inherit checkPhase; }
