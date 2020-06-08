args@{ ... }:
let pkgs-k = "pkgs"; utils-k = "utils"; config-k = "config"; in
assert let k = pkgs-k;  in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
assert let k = utils-k; in builtins.hasAttr k args -> builtins.isAttrs args."${k}";
let
  pkgs = args."${pkgs-k}" or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { "${k}" = args."${k}".nixpkgs."${k}"; } else {}
  ));

  utils = args."${utils-k}" or (import ../nix-utils-pick.nix args).pkg;
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
