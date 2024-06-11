# nix-shell configuration for the script that provies GHC with supplied dependencies.
#
# This allows REPLing and have IDE-like features in my Vim for the script.
#
# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{ pkgs ? import <nixpkgs> {}

, __srcFile ? ./main.hs

, inNixShell ? false # Set automatically to “true” when running from nix-shell
}:

let
  esc = pkgs.lib.escapeShellArg;

  ghc = pkgs.haskellPackages.ghcWithPackages (p: [
    p.bytestring
    p.text
    p.vector
    p.containers
    p.aeson
    p.toml-parser
  ]);

  inherit (pkgs) haskell-language-server;

  shell = pkgs.mkShell {
    buildInputs = [
      ghc
      haskell-language-server # LSP
    ];
  };

  clunky-toml-json-converter =
    let name = "clunky-toml-json-converter"; in
    pkgs.runCommand name {} ''
      set -o nounset
      mkdir -p -- "$out/bin"
      executable="$out/bin/"${esc name}
      ${esc ghc}/bin/ghc -O2 -o "$executable" ${esc "${__srcFile}"}
      ${esc pkgs.coreutils}/bin/chmod +x -- "$executable"
    '';
in

(if inNixShell then shell else clunky-toml-json-converter) // {
  inherit
    clunky-toml-json-converter
    shell
    haskell-language-server
    ;
}
