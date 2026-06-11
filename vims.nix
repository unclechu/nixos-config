# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import nix/sources.nix; in
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

    inherit neovide;
  });

  neovide = pkgs.callPackage apps/neovide.nix {};

  wenzels-neovim = pkgs.callPackage apps/wenzels-neovim.nix {
    inherit (pkgs) neovim-unwrapped;
    inherit neovide bashEnvFile;
  };

  pluginsLackingLicenseInformation =
    import "${sources.neovimrc}/nix/plugins/plugins-lacking-license-information.nix";

  configuration = {
    environment.systemPackages = [
      system-vim.vim
      system-vim.neovim
      system-vim.neovide
      pkgs.neovim-remote
    ];

    users.users.${wenzelUserName}.packages = [
      wenzels-neovim.neovim
      wenzels-neovim.neovide
      wenzels-neovim.scripts.clean-vim
      wenzels-neovim.scripts.git-grep-nvr
      wenzels-neovim.scripts.nvimd
    ];

    unfreePredicates = [
      # Allow some “unfree” Vim plugins that are marked as “unfree” due to
      # lacking any license information.
      (pkg:
        lib.pipe pluginsLackingLicenseInformation [
          builtins.attrNames
          (builtins.any (name: lib.getName pkg == name))
        ] && pkg.meta.license == lib.licenses.unfree
      )
    ];
  };
in
{
  inherit configuration;
}
