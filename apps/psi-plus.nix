# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ fetchFromGitHub, psi-plus }:

(psi-plus.override { enablePsiMedia = true; }).overrideAttrs (_: {
  version = "1.5.1582";

  src = fetchFromGitHub {
    owner = "psi-plus";
    repo = "psi-plus-snapshots";
    rev = "92fe048134c24cc9f3dc21d83cca12587fc81dea"; # 15 November 2021
    sha256 = "0vmccxrs8ga4s2xzj00xijk8azyihycg1fp5bfgqvf833hdp3hk4";
  };
})
