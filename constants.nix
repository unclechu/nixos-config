# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

let constants = builtins.fromTOML (builtins.readFile ./constants.toml); in

constants // {
  systemProfile =
    builtins.foldl'
      (acc: name: acc // { ${name} = name; })
      {}
      constants.systemProfiles;
}
