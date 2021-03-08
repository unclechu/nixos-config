{ pkgs        ? import <nixpkgs> {}
, bashEnvFile ? null
}:
assert ! isNull bashEnvFile -> builtins.isString bashEnvFile;
let
  inherit (import ../nix/sources.nix) neovimrc;
  generic = import "${neovimrc}/nix/generic.nix" { inherit pkgs bashEnvFile; };
in
{
  neovim         = import "${neovimrc}/nix/apps/neovim.nix"    { inherit pkgs bashEnvFile; };
  neovim-qt      = import "${neovimrc}/nix/apps/neovim-qt.nix" { inherit pkgs bashEnvFile; };
  neovim-for-gui = generic.wenzelsNeovimGeneric { forGUI = true; };

  scripts = {
    clean-vim    = import "${neovimrc}/nix/scripts/clean-vim.nix"    { inherit pkgs; };
    git-grep-nvr = import "${neovimrc}/nix/scripts/git-grep-nvr.nix" { inherit pkgs; };
    nvimd        = import "${neovimrc}/nix/scripts/nvimd.nix"        { inherit pkgs; };
  };
}
