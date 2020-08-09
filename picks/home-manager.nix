let
  commit = "318bc0754ed6370cfcae13183a7f13f7aa4bc73f"; # ref "release-20.03", 5 July 2020
in
fetchTarball {
  url = "https://github.com/rycee/home-manager/archive/${commit}.tar.gz";
  sha256 = "0hgn85yl7gixw1adjfa9nx8axmlpw5y1883lzg3zigknx6ff5hsr";
}
