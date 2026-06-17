# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{ patchance }:

# Fix “ModuleNotFoundError: No module named 'cgitb'” error
patchance.overrideAttrs (old: {
  postPatch = ''
    substituteInPlace src/patchbay/patchcanvas/portgroup_widget.py \
      --replace-fail 'from cgitb import text' ""
  '';
})
