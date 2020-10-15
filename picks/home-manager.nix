let
  # ref "release-20.09", 12 October 2020
  commit = "7339784e07217ed0232e08d1ea33b610c94657d8";
in
fetchTarball {
  url = "https://github.com/nix-community/home-manager/archive/${commit}.tar.gz";
  sha256 = "0fjisa71cjagckj1ba7nn6wb11sdp2m43y36w5pwja456ii1i7si";
}
