# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../nix/sources.nix; in
{ lib
, callPackage
, skim

# Overridable dependencies
, __bashrc             ? sources.bashrc
, __skim-shell-scripts ? callPackage "${sources.bashrc}/nix/integrations/skim-shell-scripts.nix" {}
}:
let
  miscSetups = dirEnvVarName: ''
    . "''$${dirEnvVarName}/misc/setups/fuzzy-finder.bash"
    . ${lib.escapeShellArg __skim-shell-scripts}/completion.bash
    . ${lib.escapeShellArg __skim-shell-scripts}/key-bindings.bash
    . "''$${dirEnvVarName}/misc/setups/skim-fix.bash"
    . "''$${dirEnvVarName}/misc/setups/direnv.bash"
  '';

  miscAliases = dirEnvVarName: ''
    . "''$${dirEnvVarName}/misc/aliases/skim.bash"
    . "''$${dirEnvVarName}/misc/aliases/fuzzy-finder.bash"
    . "''$${dirEnvVarName}/misc/aliases/nvr.bash"
    . "''$${dirEnvVarName}/misc/aliases/tmux.bash"
    . "''$${dirEnvVarName}/misc/aliases/gpg.bash"
    . "''$${dirEnvVarName}/misc/aliases/gpaste.bash"
  '';
in
callPackage __bashrc {
  inherit miscSetups miscAliases;
} // {
  inherit miscSetups miscAliases;
}
