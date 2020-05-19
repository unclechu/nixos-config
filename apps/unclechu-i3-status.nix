args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, ...
}:
let
  utils = import ../utils args;
  inherit (utils) esc writeCheckedExecutable nameOfModuleFile;

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

  dash = "${pkgs.dash}/bin/dash";
  dzen2 = "${pkgs.dzen2}/bin/dzen2";

  checkPhase = ''
    ${utils.bash.checkFileIsExecutable dash}
    ${utils.bash.checkFileIsExecutable dzen2}
    ${utils.bash.checkFileIsExecutable pkg-exe}
  '';

  wrapper = writeCheckedExecutable name checkPhase ''
    #! ${dash}
    PATH=${pkgs.dzen2}/bin:$PATH ${esc pkg-exe} "$@"
  '';
in
{
  inherit name src;
  haskell-pkg = pkg;
  pkg = wrapper;
  haskellPackages = hs;
}
