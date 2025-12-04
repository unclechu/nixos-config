# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ pkgs
, lib

# Option for “neovimrc” config
, bashEnvFile
}:
let
  inherit (import ./constants.nix) wenzelUserName;

  system-vim = lib.fix (r: {
    vim = pkgs.vim-full.customize {
      name = "vim";
      vimrcConfig.packages.myplugins = {
        start = [ pkgs.vimPlugins.vim-nix ];
        opt = [];
      };
      vimrcConfig.customRC = ''
        set nocompatible
        set hidden
        syntax on
      '';
    };

    neovim = pkgs.neovim.override {
      configure.packages.myPlugins = {
        start = [ pkgs.vimPlugins.vim-nix ];
        opt = [];
      };
    };

    neovim-qt = pkgs.neovim-qt.override { neovim = r.neovim; };
    inherit neovide;
  });

  neovide = pkgs.callPackage apps/neovide.nix {};

  wenzels-neovim = pkgs.callPackage apps/wenzels-neovim.nix {
    inherit (pkgs) neovim;
    inherit neovide bashEnvFile;
  };

  configuration = {
    environment.systemPackages = [
      system-vim.vim
      system-vim.neovim
      system-vim.neovim-qt
      system-vim.neovide
      pkgs.neovim-remote
    ];

    users.users.${wenzelUserName}.packages = [
      wenzels-neovim.neovim
      wenzels-neovim.neovim-qt
      wenzels-neovim.neovide
      wenzels-neovim.scripts.clean-vim
      wenzels-neovim.scripts.git-grep-nvr
      wenzels-neovim.scripts.nvimd
    ];
  };
in
{
  inherit configuration;
}
