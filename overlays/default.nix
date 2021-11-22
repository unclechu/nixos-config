# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
[
  (import ./amdgpu.nix)
  (import ./neovim-qt.nix)
  (import ./dino.nix)
]
