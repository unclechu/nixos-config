args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, ...
}:
let
  utils = import ../utils args;
  inherit (utils) esc wrapExecutable nameOfModuleFile;

  src = fetchGit {
    url = "https://github.com/unclechu/unclechu-i3-status.git";
    rev = "848170821a9499c84772c7df793888ce45bfa999"; # 8 March 2020
    ref = "master";
  };

  hs = pkgs.haskellPackages.extend (self: super: {
    base-unicode-symbols = super.base-unicode-symbols_0_2_4_2;
  });

  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  pkg = hs.callCabal2nix name src {};
  pkg-exe = "${pkgs.haskell.lib.justStaticExecutables pkg}/bin/${name}";

  dzen2 = "${pkgs.dzen2}/bin/dzen2";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable dzen2}
    ${utils.shellCheckers.fileIsExecutable pkg-exe}
  '';

  wrapper = wrapExecutable pkg-exe { deps = [ pkgs.dzen2 ]; inherit checkPhase; };
in
{
  inherit name src;
  haskell-pkg = pkg;
  pkg = wrapper;
  haskellPackages = hs;
}
