args@{ ... }:
let pkgs-k = "pkgs"; utils-k = "utils"; config-k = "config"; in
assert let k = pkgs-k;  in builtins.hasAttr k args -> builtins.isAttrs args.${k};
assert let k = utils-k; in builtins.hasAttr k args -> builtins.isAttrs args.${k};
let
  utils = args.${utils-k} or (import ../picks/nix-utils.nix (
    pkgs.lib.filterAttrs (k: _: k == config-k) args // { inherit pkgs; }
  )).pkg;

  inherit (utils) esc;

  bashRC = pkgs.fetchFromGitHub {
    owner = "unclechu";
    repo = "bashrc";
    rev = "4479fea368d371ffa97ed74f530e006224a0cd2d"; # ref "master", 3 November 2020
    sha256 = "0zcbz1ypdqyv80z0jqgr4yr99nkqicb255pl6wwf7lc8sm48gszi";
  };

  pkgs = args.${pkgs-k} or (import <nixpkgs> (
    let k = config-k; in
    if builtins.hasAttr k args then { ${k} = args.${k}.nixpkgs.${k}; } else {}
  ));

  wenzel-nixos-pc        = import ../hardware/wenzel-nixos-pc.nix args;
  rw-wenzel-nixos-laptop = import ../hardware/rw-wenzel-nixos-laptop.nix args;
  hostName               = args."${config-k}".networking.hostName or null;

  miscSetups = dirEnvVarName:
    if hostName == wenzel-nixos-pc.networking.hostName
    || hostName == rw-wenzel-nixos-laptop.networking.hostName
    then ''
      # miscellaneous setups
      . "''$${dirEnvVarName}/misc/setups/fuzzy-finder.bash"
      . ${esc pkgs.skim}/share/skim/completion.bash
      . ${esc pkgs.skim}/share/skim/key-bindings.bash
    ''
    else "";

  miscAliases = dirEnvVarName:
    if hostName == wenzel-nixos-pc.networking.hostName
    || hostName == rw-wenzel-nixos-laptop.networking.hostName
    then ''
      # miscellaneous aliases
      . "''$${dirEnvVarName}/misc/aliases/skim.bash"
      . "''$${dirEnvVarName}/misc/aliases/fuzzy-finder.bash"
      . "''$${dirEnvVarName}/misc/aliases/nvr.bash"
    ''
    else "";
in
import bashRC {
  inherit pkgs miscSetups miscAliases;
} // {
  inherit miscSetups miscAliases hostName;
}
