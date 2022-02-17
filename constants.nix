# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{
  wenzelUserName            = "wenzel";
  rawdevinputGroupName      = "rawdevinput";
  backlightcontrolGroupName = "backlightcontrol";
  jackaudioGroupName        = "jackaudio";
  audioGroupName            = "audio";

  keyRepeat = {
    delay    = 170;
    interval = 30;
  };

  xkb = {
    layout  = "us,ru,fi";
    options = "eurosign:e,grp:shifts_toggle";
  };

  systemProfile = builtins.foldl' (acc: name: acc // { ${name} = name; }) {} [
    "default"
    "audio"
  ];
}
