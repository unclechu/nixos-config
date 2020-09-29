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
    rev = "4d7190a9acd994ff26262eb31a522411305fe604"; # ref "master", 30 September 2020
    sha256 = "1h8hjaq53dqf95r419m644bxg2lkl9v8r2nydv4ifnmap3rrcn5w";
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
