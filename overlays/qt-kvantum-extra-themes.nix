# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

self: super:

let
  esc = super.lib.escapeShellArg;

  addThemesPatch =
    { pkg # Theme source package
    , themes # Attrset where name is theme name and value is source dir to copy the theme files from
    }:
    assert super.lib.isDerivation pkg;
    assert builtins.isAttrs themes;
    assert builtins.all builtins.isString (builtins.attrValues themes);
    super.lib.pipe themes [
      super.lib.attrsToList
      (map ({ name, value }: ''
        mkdir -- themes/kvthemes/${esc name}
        cp -- ${esc pkg}/${esc value}/${esc name}.kvconfig themes/kvthemes/${esc name}
        cp -- ${esc pkg}/${esc value}/${esc name}.svg themes/kvthemes/${esc name}
      ''))
      (builtins.concatStringsSep "\n")
    ];

  kvantumOverride = old: {
    postPatch = ''
      ${old.postPatch}

      # KvLibadwaita
      ${addThemesPatch {
        pkg = super.fetchFromGitHub {
          owner = "GabePoel";
          repo = "KvLibadwaita";
          # 5 August 2022
          rev = "61f2e0b04937b6d31f0f4641c9c9f1cc3600a723";
          hash = "sha256-65Gz3WNAwuoWWbBZJL0Ifl+PVLOHjpl6GNhR1oVmGZ0=";
        };
        themes = {
          KvLibadwaita = "src/KvLibadwaita";
          KvLibadwaitaDark = "src/KvLibadwaita";
        };
      }}

      # Vivid-Dark-Kvantum
      ${addThemesPatch {
        pkg = super.fetchFromGitHub {
          owner = "L4ki";
          repo = "Vivid-Plasma-Themes";
          # 29 November 2023
          rev = "a049363b24618900084633e2b9af2298abb17472";
          hash = "sha256-vXwqLTNXUavPxUqv+SMgqLwUujzt9XWFVeofMEHZWwc=";
        };
        themes = { Vivid-Dark-Kvantum = "Vivid Kvantum Themes/Vivid-Dark-Kvantum"; };
      }}

      # Otto
      ${addThemesPatch {
        pkg = super.fetchFromGitLab {
          owner = "jomada";
          repo = "otto";
          # 28 October 2023
          rev = "16d5a62a3af67dad5d4c975e1773d78a4111a043";
          hash = "sha256-6qR62CLLvQxnvcfAvmG956DTMUiD8A/PaVI6fK2LuFg=";
        };
        themes = { Otto = "kvantum/Otto"; };
      }}
    '';
  };
in

{
  libsForQt5 = super.libsForQt5.overrideScope (qtSelf: qtSuper: {
    qtstyleplugin-kvantum = qtSuper.qtstyleplugin-kvantum.overrideAttrs kvantumOverride;
  });

  qt6Packages = super.qt6Packages.overrideScope (qtSelf: qtSuper: {
    qtstyleplugin-kvantum = qtSuper.qtstyleplugin-kvantum.overrideAttrs kvantumOverride;
  });
}
