# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
args@{ pkgs, lib, ... }:
let
  sources = import nix/sources.nix;

  inherit (import ./constants.nix)
    wenzelUserName
    rawdevinputGroupName backlightcontrolGroupName jackaudioGroupName audioGroupName;

  inherit ((import ./my-packages.nix args).my-apps) wenzels-bash;
in
{
  imports = [
    (import "${sources.home-manager}/nixos")
    user-specific/dunst.nix
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
    };
  };

  home-manager.users.${wenzelUserName} = {
    programs.git = {
      enable = true;
      userName = "Viacheslav Lotsmanov";
      userEmail = "lotsmanov89@gmail.com";

      signing = {
        signByDefault = true;
        key = null;
      };
    };

    home.file.".bashrc".text = ''
      . ${lib.escapeShellArg wenzels-bash.history-settings-file-path}
    '';
  };
}
