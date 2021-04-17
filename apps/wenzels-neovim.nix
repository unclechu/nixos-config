# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ callPackage

# Build options
, bashEnvFile ? null
}:
assert ! isNull bashEnvFile -> builtins.isString bashEnvFile;
let
  inherit (sources) neovimrc;
  generic = callPackage "${neovimrc}/nix/generic.nix" { inherit bashEnvFile; };
in
rec {
  neovim    = callPackage "${neovimrc}/nix/apps/neovim.nix"    { inherit bashEnvFile; };
  neovim-qt = callPackage "${neovimrc}/nix/apps/neovim-qt.nix" { inherit bashEnvFile; };

  inherit (neovim-qt) neovim-for-gui;

  scripts = {
    clean-vim    = callPackage "${neovimrc}/nix/scripts/clean-vim.nix"    {};
    git-grep-nvr = callPackage "${neovimrc}/nix/scripts/git-grep-nvr.nix" {};
    nvimd        = callPackage "${neovimrc}/nix/scripts/nvimd.nix"        { __neovim = neovim; };
  };
}
