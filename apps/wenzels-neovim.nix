# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ callPackage

, neovim
, neovim-qt

# Build options
, bashEnvFile ? null
}:
assert ! isNull bashEnvFile -> builtins.isString bashEnvFile;
let
  inherit (sources) neovimrc;

  wenzels-neovim =
    callPackage "${neovimrc}/nix/apps/neovim.nix" { inherit neovim bashEnvFile; };
  wenzels-neovim-qt =
    callPackage "${neovimrc}/nix/apps/neovim-qt.nix" { inherit neovim neovim-qt bashEnvFile; };
in
{
  neovim    = wenzels-neovim;
  neovim-qt = wenzels-neovim-qt;

  inherit (wenzels-neovim-qt) neovim-for-gui;

  scripts = {
    clean-vim    = callPackage "${neovimrc}/nix/scripts/clean-vim.nix"    {};
    git-grep-nvr = callPackage "${neovimrc}/nix/scripts/git-grep-nvr.nix" {};
    nvimd        = callPackage "${neovimrc}/nix/scripts/nvimd.nix"        { __neovim = wenzels-neovim; };
  };
}
