# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ bibata-cursors }:

bibata-cursors.overrideAttrs (old:
  let inherit (old) bitmaps; in {
    buildPhase = ''
      runHook preBuild

      ctgen build.right.toml -p x11 -d $bitmaps/Bibata-Modern-Amber-Right -n 'Bibata-Modern-Amber-Right' -c 'Yellowish and rounded edge bibata left-handed cursors.'
      ctgen build.right.toml -p x11 -d $bitmaps/Bibata-Modern-Classic-Right -n 'Bibata-Modern-Classic-Right' -c 'Black and rounded edge Bibata left-handed cursors.'
      ctgen build.right.toml -p x11 -d $bitmaps/Bibata-Modern-Ice-Right -n 'Bibata-Modern-Ice-Right' -c 'White and rounded edge Bibata left-handed cursors.'

      ctgen build.right.toml -p x11 -d $bitmaps/Bibata-Original-Amber-Right -n 'Bibata-Original-Amber-Right' -c 'Yellowish and sharp edge Bibata left-handed cursors.'
      ctgen build.right.toml -p x11 -d $bitmaps/Bibata-Original-Classic-Right -n 'Bibata-Original-Classic-Right' -c 'Black and sharp edge Bibata left-handed cursors.'
      ctgen build.right.toml -p x11 -d $bitmaps/Bibata-Original-Ice-Right -n 'Bibata-Original-Ice-Right' -c 'White and sharp edge Bibata left-handed cursors.'

      runHook postBuild
    '';
  }
)
