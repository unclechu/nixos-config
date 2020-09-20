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
    rev = "5aa02fd671b2a1b4b89afbc3ba3cd444b5884665"; # ref "master", 21 September 2020
    sha256 = "1krmcxl5lp05203dkvmy1k7xgs0rkpm521pf2c3bw2vzbw4rj1g2";
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
