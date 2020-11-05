let
  # ref "release-20.09", 4 November 2020
  commit = "63f299b3347aea183fc5088e4d6c4a193b334a41";
in
fetchTarball {
  url = "https://github.com/nix-community/home-manager/archive/${commit}.tar.gz";
  sha256 = "0iksjch94wfvyq0cgwv5wq52j0dc9cavm68wka3pahhdvjlxd3js";
}
