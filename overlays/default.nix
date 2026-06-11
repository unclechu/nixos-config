# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
[
  (import ./qt-kvantum-extra-themes.nix)
  (import ./fix-qm-interpolated-string-hs-pkg.nix)
  (import ./skim.nix)
]
