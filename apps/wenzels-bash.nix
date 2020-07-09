args@{ ... }:
let pkgs-k = "pkgs"; utils-k = "utils"; config-k = "config"; in
assert let k = pkgs-k;  in builtins.hasAttr k args -> builtins.isAttrs  args.${k};
assert let k = utils-k; in builtins.hasAttr k args -> builtins.isAttrs args.${k};
let
  utils = args.${utils-k} or (import ../nix-utils-pick.nix (
    pkgs.lib.filterAttrs (k: _: k == config-k) args // { inherit pkgs; }
  )).pkg;

  inherit (utils) esc;

  bashRC = fetchGit {
    url = "https://github.com/unclechu/bashrc.git";
    rev = "c5e5f6cdd33c5160bad9747f2cc37010b99106d3"; # 9 July 2020
    ref = "master";
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
