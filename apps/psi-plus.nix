# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ fetchFromGitHub, psi-plus }:

(psi-plus.override { enablePsiMedia = true; }).overrideAttrs (_: {
  version = "1.5.1557-git-master"; # The commit is later than the actual version (plugins update)

  src = fetchFromGitHub {
    owner = "psi-plus";
    repo = "psi-plus-snapshots";
    rev = "1e4b513d7f8bd6de7f567137bf97bfbaa6f1b1b1"; # 30 October 2021
    sha256 = "1m45y32sslxab6g6syb23ywsw20jf70wxfn39yga5ddnafig2fpr";
  };
})
