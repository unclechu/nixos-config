args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, ...
}:
let
  inherit (import ../utils args) nameOfModuleFile;

  src = fetchGit {
    url = "https://github.com/unclechu/place-cursor-at.git";
    rev = "1a5a11550e03fb679e43940e67a9e04d6aeb515d"; # 8 March 2020
    ref = "master";
  };

  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  pkg = pkgs.haskellPackages.callCabal2nix name src {};
in
{
  inherit name src;
  haskell-pkg = pkg;
  pkg = pkgs.haskell.lib.justStaticExecutables pkg;
}
