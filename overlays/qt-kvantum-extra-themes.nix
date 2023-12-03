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

  vividDarkKvantum =
    let
      name = "Vivid-Dark-Kvantum";
      srcArchive = "${qt-kvantum-extra-themes/Vivid-Dark-Kvantum.tar.gz}";
      sha256 = "643b7c6feafe92fd931c7720782d4e93a482605044e9159f331844dbb90f8aba";
    in
    # Extract archive contents
    super.runCommand name {} ''
      set -o nounset
      set -o pipefail

      CHECKSUM=$(sha256sum -- ${esc srcArchive} | cut -d ' ' -f 1)
      if [[ $CHECKSUM != ${esc sha256} ]]; then
        >&2 printf "Checksum %s mismatches expectation: %s\n" "$CHECKSUM" ${esc sha256}
        exit 1
      fi

      OUT_THEME_DIR="$out"/${esc name}
      mkdir -p -- "$OUT_THEME_DIR"

      tar -xvf ${esc srcArchive}
      cp -- ${esc "${name}/${name}.svg"} "$OUT_THEME_DIR"
      cp -- ${esc "${name}/${name}.kvconfig"} "$OUT_THEME_DIR"
    '';

  addKvLibadwaitaTheme = ''
    mkdir themes/kvthemes/KvLibadwaita
    mkdir themes/kvthemes/KvLibadwaitaDark
    cp -- ${esc kvLibadwaita}/src/KvLibadwaita/KvLibadwaita.kvconfig themes/kvthemes/KvLibadwaita
    cp -- ${esc kvLibadwaita}/src/KvLibadwaita/KvLibadwaita.svg themes/kvthemes/KvLibadwaita
    cp -- ${esc kvLibadwaita}/src/KvLibadwaita/KvLibadwaitaDark.kvconfig themes/kvthemes/KvLibadwaitaDark
    cp -- ${esc kvLibadwaita}/src/KvLibadwaita/KvLibadwaitaDark.svg themes/kvthemes/KvLibadwaitaDark
  '';

  # Downloaded the archive from this page: https://store.kde.org/p/2110194
  # File was dated 2023-11-29.
  addVividDarkKvantumTheme = ''
    mkdir themes/kvthemes/Vivid-Dark-Kvantum
    cp -- ${esc vividDarkKvantum}/Vivid-Dark-Kvantum/Vivid-Dark-Kvantum.kvconfig themes/kvthemes/Vivid-Dark-Kvantum
    cp -- ${esc vividDarkKvantum}/Vivid-Dark-Kvantum/Vivid-Dark-Kvantum.svg themes/kvthemes/Vivid-Dark-Kvantum
  '';

  kvantumOverride = old: {
    postPatch = ''
      ${old.postPatch}
      ${addKvLibadwaitaTheme}
      ${addVividDarkKvantumTheme}
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
