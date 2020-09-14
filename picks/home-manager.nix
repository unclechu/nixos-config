let
  # ref "master" (20.09 isn’t released yet), 13 September 2020
  commit = "472ca211cac604efdf621337067a237be9df389e";
in
fetchTarball {
  url = "https://github.com/nix-community/home-manager/archive/${commit}.tar.gz";
  sha256 = "1gbfsnd7zsxwqryxd4r6jz9sgdz6ghlkapws1cdxshrbxlwhqad1";
}
