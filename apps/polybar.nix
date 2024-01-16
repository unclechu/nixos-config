# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{ polybarFull }:

polybarFull.override {
  alsaSupport = false;
  githubSupport = false;
  mpdSupport = false;

  pulseSupport = true;
  iwSupport = true;
  nlSupport = true;
  i3Support = true;
}
