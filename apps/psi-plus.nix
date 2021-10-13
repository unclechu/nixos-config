# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ fetchFromGitHub, psi-plus }:

(psi-plus.override { enablePsiMedia = true; }).overrideAttrs (_: {
  version = "1.5.1556";

  src = fetchFromGitHub {
    owner = "psi-plus";
    repo = "psi-plus-snapshots";
    # FIXME two commits, one version
    rev = "635879010b6697f7041a7bbea1853a1f4673c7f7"; # 12 October 2021
    sha256 = "18xvljcm0a9swkyz4diwxi4xaj0w27jnhfgpi8fv5fj11j0g1b3a";
  };
})
