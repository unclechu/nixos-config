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

      # Canta
      ${addThemesPatch {
        pkg = super.fetchFromGitHub {
          owner = "vinceliuice";
          repo = "Canta-kde";
          # 17 May 2022
          rev = "ec7e792368e2253dbe24d220ddf1ded64d0b9718";
          hash = "sha256-oZiZihs/45UBirUyabrG23xfOx+cqkR95rCScgQIzgg=";
        };
        themes = {
          Canta-dark = "Kvantum/Canta-dark";
          Canta-light = "Kvantum/Canta-light";
        };
      }}

      # Sweet
      ${addThemesPatch {
        pkg = super.fetchFromGitHub {
          owner = "EliverLara";
          repo = "Sweet";
          # 27 November 2023
          rev = "f6e53f1f32d206d3446ab4ce8cc1f7bffa5f76e6";
          hash = "sha256-8bN9KDrblRIAh8J461j8H1xGWI03/3jTAwwoUPdvqoE=";
        };
        themes = {
          Sweet = "kde/Kvantum/Sweet";
          Sweet-transparent-toolbar = "kde/Kvantum/Sweet-transparent-toolbar";
        };
      }}

      # Fluent
      ${addThemesPatch {
        pkg = super.fetchFromGitHub {
          owner = "Luwx";
          repo = "Fluent-kvantum";
          # 12 Jul 2019
          rev = "890a45051f735aa7ee05aa734103fb634fb496eb";
          hash = "sha256-+yqpdkW6QzQZtdiw2ebST+JNNLrQ2l3u+8f9NjeyRJw=";
        };
        themes = {
          Fluent-Dark = "Fluent-Dark";
          Fluent-Light = "Fluent-Light";
        };
      }}

      # Utterly-Sweet
      ${addThemesPatch {
        pkg = super.fetchFromGitHub {
          owner = "HimDek";
          repo = "Utterly-Sweet-Plasma";
          # 15 April 2023
          rev = "4bb007e865d559de8dd43bbffb93778ea136f957";
          hash = "sha256-oEyf6FI5XSjXPZjzBiGypwZY89ulhwAwk9QIJ3pMw/M=";
        };
        themes = {
          Utterly-Sweet = "kvantum";
          Utterly-Sweet-Solid = "kvantum-solid";
        };
      }}

      # KDE-Story-Blue-Dark-Kvantum
      ${addThemesPatch {
        pkg = super.fetchFromGitHub {
          owner = "L4ki";
          repo = "KDE-Story-Blue-Plasma-Themes";
          # 2 December 2023
          rev = "fd06f720c0b60710ad99f268aa7074202e36ebf7";
          hash = "sha256-TMu+T0G2p8JsRmFrsMuLYc5jrCxxh0fP9Z1riCLUND8=";
        };
        themes = {
          KDE-Story-Blue-Dark-Kvantum = "KDE-Story-Blue Kvantum Themes/KDE-Story-Blue-Dark-Kvantum";
        };
      }}

      # Sumac
      ${addThemesPatch {
        pkg = super.fetchFromGitHub {
          owner = "doncsugar";
          repo = "sumac-theme";
          # 13 November 2023
          rev = "9cec51ab1ab0d0a52b921dfe48f65b4e9b3a0fd7";
          hash = "sha256-nZtX8k9XAxpCeZ6VWgdbyndvVsmZAT8YPZ8xZkSEdQc=";
        };
        themes = {
          Sumac = "kvantum/Sumac";
          SumacDark = "kvantum/Sumac";
          SumacDoncsugar = "kvantum/SumacDoncsugar";
          SumacDoncsugarDark = "kvantum/SumacDoncsugar";
          Summaculate = "kvantum/Summaculate";
          SummaculateDark = "kvantum/Summaculate";
        };
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
