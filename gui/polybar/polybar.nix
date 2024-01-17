# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

{ polybarFull

, lib

# Pick sources from the Polybar locally cloned repo.
, useLocalSources ? false

, localSources ? lib.cleanSource ./polybar-src
}:

let
  polybar = polybarFull.override {
    alsaSupport = false;
    githubSupport = false;
    mpdSupport = false;

    pulseSupport = true;
    iwSupport = true;
    nlSupport = true;
    i3Support = true;
  };

  addPatches =
    if useLocalSources then lib.id else
    x: x.overrideAttrs (old: {
      patches = old.patches ++ [
        ./xkeyboard-my-custom-layout.patch
      ];
    })
    ;

  overrideSources =
    if ! useLocalSources then lib.id else
    x: x.overrideAttrs (old: {
      version = "local-git";
      src = localSources;
    })
    ;
in

lib.pipe polybar [
  overrideSources
  addPatches
]
