# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

self: super:

let
  esc = super.lib.escapeShellArg;

  kvLibadwaita = super.fetchFromGitHub {
    owner = "GabePoel";
    repo = "KvLibadwaita";
    rev = "61f2e0b04937b6d31f0f4641c9c9f1cc3600a723";
    hash = "sha256-65Gz3WNAwuoWWbBZJL0Ifl+PVLOHjpl6GNhR1oVmGZ0=";
  };

  addKvLibadwaitaThemeOverride = old: {
    postPatch = ''
      ${old.postPatch}
      mkdir themes/kvthemes/KvLibadwaita
      mkdir themes/kvthemes/KvLibadwaitaDark
      cp -- ${esc kvLibadwaita}/src/KvLibadwaita/KvLibadwaita.kvconfig themes/kvthemes/KvLibadwaita
      cp -- ${esc kvLibadwaita}/src/KvLibadwaita/KvLibadwaita.svg themes/kvthemes/KvLibadwaita
      cp -- ${esc kvLibadwaita}/src/KvLibadwaita/KvLibadwaitaDark.kvconfig themes/kvthemes/KvLibadwaitaDark
      cp -- ${esc kvLibadwaita}/src/KvLibadwaita/KvLibadwaitaDark.svg themes/kvthemes/KvLibadwaitaDark
    '';
  };
in

{
  libsForQt5 = super.libsForQt5.overrideScope' (qtSelf: qtSuper: {
    qtstyleplugin-kvantum =
      qtSuper.qtstyleplugin-kvantum.overrideAttrs addKvLibadwaitaThemeOverride;
  });

  qt6Packages = super.qt6Packages.overrideScope' (qtSelf: qtSuper: {
    qtstyleplugin-kvantum =
      qtSuper.qtstyleplugin-kvantum.overrideAttrs addKvLibadwaitaThemeOverride;
  });
}
