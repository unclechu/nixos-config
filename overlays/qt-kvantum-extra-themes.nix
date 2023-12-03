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

  vividDarkKvantum = fetchThemeTarballFromPath {
    name = "Vivid-Dark-Kvantum";
    # Downloaded the archive from this page: https://store.kde.org/p/2110194
    # File is dated 2023-11-29
    srcPath = qt-kvantum-extra-themes/Vivid-Dark-Kvantum.tar.gz;
    sha256 = "643b7c6feafe92fd931c7720782d4e93a482605044e9159f331844dbb90f8aba";
  };

  fetchThemeTarballFromPath =
    { name, srcPath, sha256 }:
    assert builtins.isString name;
    assert builtins.isPath srcPath;
    assert builtins.isString sha256;
    let src = "${srcPath}"; in
    # Extract archive contents
    super.runCommand name {} ''
      set -o nounset
      set -o pipefail

      CHECKSUM=$(sha256sum -- ${esc src} | cut -d ' ' -f 1)
      if [[ $CHECKSUM != ${esc sha256} ]]; then
        >&2 printf "Checksum %s mismatches expectation: %s\n" "$CHECKSUM" ${esc sha256}
        exit 1
      fi

      OUT_THEME_DIR="$out"/${esc name}
      mkdir -p -- "$OUT_THEME_DIR"

      tar -xvf ${esc src}
      cp -- ${esc "${name}/${name}.svg"} "$OUT_THEME_DIR"
      cp -- ${esc "${name}/${name}.kvconfig"} "$OUT_THEME_DIR"
    '';

  addThemePatch =
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

      ${addThemePatch {
        pkg = kvLibadwaita;
        themes = {
          KvLibadwaita = "src/KvLibadwaita";
          KvLibadwaitaDark = "src/KvLibadwaita";
        };
      }}

      ${addThemePatch {
        pkg = vividDarkKvantum;
        themes = { Vivid-Dark-Kvantum = "Vivid-Dark-Kvantum"; };
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
