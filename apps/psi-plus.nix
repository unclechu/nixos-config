# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
{ fetchFromGitHub, psi-plus }:

(psi-plus.override { enablePsiMedia = true; }).overrideAttrs (_: rec {
  version = "1.5.1588";

  src = fetchFromGitHub {
    owner = "psi-plus";
    repo = "psi-plus-snapshots";
    rev = version;
    sha256 = "1qylmq4spay71c6q9xrm311vx54iji1ga4ij07k51598fg5dba8d";
  };
})
