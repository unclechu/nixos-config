let
  # ref "master" (20.09 isn’t released yet), 21 September 2020
  commit = "96d7de6db18d9a5bf254ddf3525bb4ef1d2a6bda";
in
fetchTarball {
  url = "https://github.com/nix-community/home-manager/archive/${commit}.tar.gz";
  sha256 = "0dc4zji0vhmlbnx9fh5iihd3jbn0j2knq5s6ryly70k7mnzrm4ml";
}
