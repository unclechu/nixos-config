# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
args@{ pkgs, lib, ... }:
let
  sources = import nix/sources.nix;

  inherit (import ./constants.nix)
    wenzelUserName
    rawdevinputGroupName
    backlightcontrolGroupName
    cpumodecontrolGroupName
    jackaudioGroupName
    audioGroupName
    ;

  inherit ((import ./my-packages.nix args).my-apps) wenzels-bash;
in
{
  imports = [
    (import "${sources.home-manager}/nixos")
    user-specific/dunst.nix
    user-specific/mouse-cursor
    user-specific/gui.nix
  ];

  users = {
    # See also “users.users.${wenzelUserName}.packages” in “my-packages.nix”
    users.${wenzelUserName} = {
      uid = 1989;
      isNormalUser = true;
      group = wenzelUserName;
      shell = wenzels-bash;

      extraGroups = [
        "users"
        "wheel"
        "networkmanager"
        rawdevinputGroupName
        backlightcontrolGroupName
        cpumodecontrolGroupName
        "docker"
        "vboxusers"
        jackaudioGroupName
        audioGroupName
      ];
    };

    groups = {
      ${wenzelUserName}.gid = 1989;
      ${rawdevinputGroupName}.gid = 500;
      ${backlightcontrolGroupName}.gid = 501;
      ${cpumodecontrolGroupName}.gid = 502;
    };
  };

  home-manager.users.${wenzelUserName} = {
    home.stateVersion = "22.05";

    programs.git = {
      enable = true;
      userName = "Viacheslav Lotsmanov";
      userEmail = "lotsmanov89@gmail.com";

      signing = {
        signByDefault = true;
        key = null;
      };

      extraConfig = {
        # Enable support for git-subtrac kind of techniques for managing Git
        # Submodules. When submodule path is set to the same repo (e.g. “.”)
        # Git fails to do git-submodule-update with an error like this:
        # “fatal: transport 'file' not allowed”. This fixes this problem.
        # See also https://github.com/apenwarr/git-subtrac
        protocol.file.allow = "always";
      };
    };

    home.file.".bashrc".text = ''
      . ${lib.escapeShellArg wenzels-bash.history-settings-file-path}
    '';
  };
}
