# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ callPackage
, lib
, skim

, # System config (e.g. self-reference) to extract machine host name.
  # Set to “null” to use in Nix REPL.
  systemConfig

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}
, __bashrc    ? sources.bashrc
}:
let
  inherit (__nix-utils) esc;

  wenzel-nixos-pc        = callPackage ../hardware/wenzel-nixos-pc.nix        {};
  rw-wenzel-nixos-laptop = callPackage ../hardware/rw-wenzel-nixos-laptop.nix {};
  hostName               = systemConfig.networking.hostName or null;

  miscSetups = dirEnvVarName:
    if hostName == wenzel-nixos-pc.networking.hostName
    || hostName == rw-wenzel-nixos-laptop.networking.hostName
    then ''
      . "''$${dirEnvVarName}/misc/setups/fuzzy-finder.bash"
      . ${esc skim}/share/skim/completion.bash
      . ${esc skim}/share/skim/key-bindings.bash
    ''
    else "";

  miscAliases = dirEnvVarName:
    if hostName == wenzel-nixos-pc.networking.hostName
    || hostName == rw-wenzel-nixos-laptop.networking.hostName
    then ''
      . "''$${dirEnvVarName}/misc/aliases/skim.bash"
      . "''$${dirEnvVarName}/misc/aliases/fuzzy-finder.bash"
      . "''$${dirEnvVarName}/misc/aliases/nvr.bash"
      . "''$${dirEnvVarName}/misc/aliases/tmux.bash"
      . "''$${dirEnvVarName}/misc/aliases/gpg.bash"
    ''
    else "";
in
callPackage __bashrc {
  inherit miscSetups miscAliases;
} // {
  inherit miscSetups miscAliases hostName;
}
