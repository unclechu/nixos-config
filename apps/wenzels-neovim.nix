# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ callPackage

, neovim-unwrapped
, neovim-qt
, neovide

# Build options
, bashEnvFile ? null
}:
assert ! isNull bashEnvFile -> builtins.isString bashEnvFile;
let
  inherit (sources) neovimrc;

  wenzels-neovim =
    callPackage "${neovimrc}/nix/apps/neovim.nix" { inherit neovim-unwrapped bashEnvFile; };
  wenzels-neovim-qt =
    callPackage "${neovimrc}/nix/apps/neovim-qt.nix" { inherit neovim-unwrapped neovim-qt bashEnvFile; };
  wenzels-neovide =
    callPackage "${neovimrc}/nix/apps/neovide.nix" { inherit neovim-unwrapped neovide bashEnvFile; };
in
{
  neovim    = wenzels-neovim;
  neovim-qt = wenzels-neovim-qt;
  neovide   = wenzels-neovide;

  inherit (wenzels-neovim-qt) neovim-for-gui;

  scripts = {
    clean-vim    = callPackage "${neovimrc}/nix/scripts/clean-vim.nix"    {};
    git-grep-nvr = callPackage "${neovimrc}/nix/scripts/git-grep-nvr.nix" {};
    nvimd        = callPackage "${neovimrc}/nix/scripts/nvimd.nix"        { __neovim = wenzels-neovim; };
  };
}
