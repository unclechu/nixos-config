args@{ ... }:
let
  pkgs-k = "pkgs"; config-k = "config"; bashEnvFile-k = "bashEnvFile";
in
assert let k = pkgs-k;        in builtins.hasAttr k args -> builtins.isAttrs  args.${k};
assert let k = bashEnvFile-k; in builtins.hasAttr k args -> builtins.isString args.${k};
let
  wenzels-neovim-src = pkgs.fetchFromGitHub {
    owner = "unclechu";
    repo = "neovimrc";
    rev = "852aa8cca403126adef6ffde489dced1deab0fee"; # ref "master", 19 November 2020
    sha256 = "1yl4nfwlj0a8429plxs3jdm24nyxiwm9akqq99b8mbv2zfz2cnhq";
  };

  pkgs = args.${pkgs-k} or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { ${k} = args.${k}.nixpkgs.${k}; } else {}
  ));

  bashEnvFile = args.${bashEnvFile-k} or null;
in
{
  neovim    = import "${wenzels-neovim-src}/nix/apps/neovim.nix"    { inherit pkgs bashEnvFile; };
  neovim-qt = import "${wenzels-neovim-src}/nix/apps/neovim-qt.nix" { inherit pkgs bashEnvFile; };

  scripts = {
    clean-vim    = import "${wenzels-neovim-src}/nix/scripts/clean-vim.nix"    { inherit pkgs; };
    git-grep-nvr = import "${wenzels-neovim-src}/nix/scripts/git-grep-nvr.nix" { inherit pkgs; };
    nvimd        = import "${wenzels-neovim-src}/nix/scripts/nvimd.nix"        { inherit pkgs; };
  };
}
