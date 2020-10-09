let
  # ref "release-20.09", 1 October 2020
  commit = "9ff2188c5d2c65b20ded3f2e7a759319866313d3";
in
fetchTarball {
  url = "https://github.com/nix-community/home-manager/archive/${commit}.tar.gz";
  sha256 = "05kmx79ms7akx5a7cay57ng8rj3ysgprfbnidb4srr46fbiyf0p2";
}
