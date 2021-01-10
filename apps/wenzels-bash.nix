{ pkgs   ? import <nixpkgs> {}
, utils  ? import (import ../nix/sources.nix).nix-utils { inherit pkgs; }

, # System config (e.g. self-reference) to extract machine host name.
  # Set to “null” to use in Nix REPL.
  config
}:
let
  inherit (utils) esc;

  # TODO Pin using “niv”
  bashRC = pkgs.fetchFromGitHub {
    owner = "unclechu";
    repo = "bashrc";
    rev = "4479fea368d371ffa97ed74f530e006224a0cd2d"; # ref "master", 3 November 2020
    sha256 = "0zcbz1ypdqyv80z0jqgr4yr99nkqicb255pl6wwf7lc8sm48gszi";
  };

  wenzel-nixos-pc        = import ../hardware/wenzel-nixos-pc.nix        { inherit pkgs; };
  rw-wenzel-nixos-laptop = import ../hardware/rw-wenzel-nixos-laptop.nix { inherit pkgs; };
  hostName               = config.networking.hostName or null;

  miscSetups = dirEnvVarName:
    if hostName == wenzel-nixos-pc.networking.hostName
    || hostName == rw-wenzel-nixos-laptop.networking.hostName
    then ''
      . "''$${dirEnvVarName}/misc/setups/fuzzy-finder.bash"
      . ${esc pkgs.skim}/share/skim/completion.bash
      . ${esc pkgs.skim}/share/skim/key-bindings.bash
    ''
    else "";

  miscAliases = dirEnvVarName:
    if hostName == wenzel-nixos-pc.networking.hostName
    || hostName == rw-wenzel-nixos-laptop.networking.hostName
    then ''
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
