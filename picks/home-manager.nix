let
  commit = "4a8d6280544d9b061c0b785d2470ad6eeda47b02"; # ref "release-20.03", 24 August 2020
in
fetchTarball {
  url = "https://github.com/rycee/home-manager/archive/${commit}.tar.gz";
  sha256 = "0m9zhp94ckzzxsgx5xdi374ndr3bh1d84344rncn9qzgnm2pzfj0";
}
