{ pkgs        ? import <nixpkgs> {}
, bashEnvFile ? null
}:
assert ! isNull bashEnvFile -> builtins.isString bashEnvFile;
let
  # TODO Pin using “niv”
  wenzels-neovim-src = pkgs.fetchFromGitHub {
    owner = "unclechu";
    repo = "neovimrc";
    rev = "db4cb18cd423a830c6804947ccb2be0a23d549bc"; # ref "master", 8 December 2020
    sha256 = "13yql3ji91hginr4bzdymkz0n0h1b5bzhylphjz7rzhfq36kp00v";
  };
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
