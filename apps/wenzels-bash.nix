args@
{ pkgs ? import <nixpkgs> { config = if builtins.hasAttr "config" args then args.config else {}; }
, config ? {}
, ...
}:
let
  utils = import ../utils args;
  inherit (utils) esc wrapExecutable nameOfModuleFile;

  src = fetchGit {
    url = "https://github.com/unclechu/bashrc.git";
    rev = "9b69adfbc095402435afc2a09d1c85ff4c6c02ed"; # 14 April 2020
    ref = "master";
  };

  dir = pkgs.runCommand "${name}-dir" {} ''
    set -Eeuo pipefail
    mkdir -- "$out"
    cp -r -- ${esc src}/misc/ "$out"
    cp -- ${esc patched-aliases-file} "$out/.bash_aliases"
  '';

  vte-sh-file = "${pkgs.vte}/etc/profile.d/vte.sh";
  wenzel-nixos-pc = import ../hardware/wenzel-nixos-pc.nix args;
  rw-wenzel-nixos-laptop = import ../hardware/rw-wenzel-nixos-laptop.nix args;
  hostName = config.networking.hostName or null;

  misc-setups =
    if hostName == wenzel-nixos-pc.networking.hostName
    || hostName == rw-wenzel-nixos-laptop.networking.hostName
    then ''
      # miscellaneous setups
      . "''$${dirEnvVarName}/misc/setups/fuzzy-finder.bash"
      . ${esc pkgs.skim}/share/skim/completion.bash
      . ${esc pkgs.skim}/share/skim/key-bindings.bash
    ''
    else "";

  misc-aliases =
    if hostName == wenzel-nixos-pc.networking.hostName
    || hostName == rw-wenzel-nixos-laptop.networking.hostName
    then ''
      # miscellaneous aliases
      . "''$${dirEnvVarName}/misc/aliases/skim.bash"
      . "''$${dirEnvVarName}/misc/aliases/fuzzy-finder.bash"
      . "''$${dirEnvVarName}/misc/aliases/nvr.bash"
    ''
    else "";

  patched-bashrc = builtins.replaceStrings [
    "'/usr/local/etc/profile.d/vte.sh'"
    "'/etc/profile.d/vte.sh'"
    "if [[ -f ~/.bash_aliases ]]; then . ~/.bash_aliases; fi"
  ] [
    (esc vte-sh-file)
    (esc vte-sh-file)
    ''
      . "''$${dirEnvVarName}/.bash_aliases"
      ${"\n" + misc-setups}
    ''
  ] (builtins.readFile "${src}/.bashrc");

  patched-aliases = ''
    ${builtins.readFile "${src}/.bash_aliases"}
    ${"\n" + misc-aliases}
  '';

  patched-bashrc-file  = pkgs.writeText "wenzels-patched-bashrc" patched-bashrc;
  patched-aliases-file = pkgs.writeText "wenzels-patched-bash-aliases" patched-aliases;

  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;
  dash = "${pkgs.dash}/bin/dash";
  bash = "${pkgs.bashInteractive_5}/bin/bash";

  checkPhase = ''
    ${utils.shellCheckers.fileIsExecutable dash}
    ${utils.shellCheckers.fileIsExecutable bash}
    ${utils.shellCheckers.fileIsReadable vte-sh-file}
  '';

  dirEnvVarName = builtins.replaceStrings ["-"] ["_"] (pkgs.lib.toUpper dir.name);

  pkg = wrapExecutable bash {
    inherit name checkPhase;
    env = { "${dirEnvVarName}" = dir; };
    args = [ "--rcfile" patched-bashrc-file ];
  };
in
assert utils.valueCheckers.isNonEmptyString patched-bashrc;
assert utils.valueCheckers.isNonEmptyString patched-aliases;
{
  inherit
    name src dir
    patched-bashrc patched-bashrc-file
    patched-aliases patched-aliases-file
    misc-setups misc-aliases
    vte-sh-file;

  pkg = pkg // { shellPath = "/bin/${name}"; };
}
