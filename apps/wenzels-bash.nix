let sources = import ../nix/sources.nix; in
{ pkgs
, nix-utils ? pkgs.callPackage sources.nix-utils {}

, # System config (e.g. self-reference) to extract machine host name.
  # Set to “null” to use in Nix REPL.
  config
}:
let
  inherit (nix-utils) esc;
  inherit (sources) bashrc;

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
import bashrc {
  inherit pkgs miscSetups miscAliases;
} // {
  inherit miscSetups miscAliases hostName;
}
