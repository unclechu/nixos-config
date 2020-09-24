#! /usr/bin/env nix-shell
#! nix-shell --pure -i raku -E
#! nix-shell "let d=[p.rakudo p.coreutils p.curl p.cacert p.libxml2];c=\"3b8ddb2f1ee6f4c0794fb6dfbd273c3599492b76\";h=\"06hgvyd8ry4i49dmjxh5n6wv1j5ifpp7i3a7bjz62san0q6d0j35\";s=fetchTarball{url=\"https://github.com/NixOS/nixpkgs/archive/${c}.tar.gz\";sha256=h;};p=import s {};in p.mkShell{buildInputs=d;}"
#↑ The pick from above is taken from branch nixos-20.09
use v6.d;
$*PROGRAM.dirname.&*chdir;

constant \channel-name           = 'nixos';
constant \distro-version         = '20.09';
constant \latest-channel-version = "https://nixos.org/channels/{channel-name}-{distro-version}";
constant \nixexprs-file          = 'nixexprs.tar.xz';
constant \git-revision-file      = 'git-revision';

sub relative(*@sub-path) { ($*CWD, channel-name, |@sub-path).join: '/' }

my Str:D \nixexprs-file-path          = relative nixexprs-file;
my Str:D \git-revision-file-path      = relative git-revision-file;
my Str:D \release-link-file-path      = relative 'release-link';
my Str:D \nixexprs-checksum-file-path = relative 'nixexprs-sha256-checksum';
my Str:D \release-date-file-path      = relative 'release-date';

sub extract-release-date(Str:D \html-page-url) of DateTime:D {
  my Str:D \html = run(«
    curl --fail --no-progress-meter -- "{html-page-url}"
  », :out).out.slurp(:close);

  with run «xmllint --format --html --xpath '//html/body/p/child::text()' -», :in, :out {
    .in.print: html;
    .in.close;
    .out.slurp(:close).chomp ~~ /
      Released\son\s
      $<date> = ( \d**4 '-' \d\d '-' \d\d ) \s
      $<time> = ( \d\d  ':' \d\d ':' \d\d )
    /;
    DateTime.new: "$<date>T$<time>"
  }
}

sub extract-nixexprs-checksum(Str:D \html-page-url) of Str:D {
  my Str:D \html = run(«
    curl --fail --no-progress-meter -- "{html-page-url}"
  », :out).out.slurp(:close);

  my Str:D @trs = do with run «
    xmllint --format --html --xpath //html/body/table/tbody/tr -
  », :in, :out {
    .in.print: html;
    .in.close;
    .out.slurp(:close).chomp.lines
  };

  my \found-hash = sub {
    do for @trs -> $tr {
      with run «
        xmllint --format --html --xpath '//tr/td/*/child::node()' -
      », :in, :out {
        .in.print: $tr;
        .in.close;
        my Str:D @lines = .out.slurp(:close).chomp.lines;
        return @lines[1] if @lines[0] eq nixexprs-file
      }
    } || Nil
  }();

  fail "SHA-256 checksum of “{nixexprs-file}” file was not found on “{html-page-url}” HTML page!"
    unless found-hash.so;

  found-hash
}

sub verify-nixexprs-file-match-checksum {
  my Str:D \checksum = nixexprs-checksum-file-path.IO.slurp.chomp;
  "Verifying that “{nixexprs-file-path}” is matching “{checksum}” checksum…".say;

  my Str:D \actual-checksum =
    run(«sha256sum -- "{nixexprs-file-path}"», :out).out.slurp(:close).chomp.split(/\s+/)[0];

  fail
    "Actual checksum of “{nixexprs-file-path}” which is “{actual-checksum}” " ~
    "doesn’t match “{checksum}” checksum from “{nixexprs-checksum-file-path}” file!"
      if actual-checksum ne checksum
}

#| Fetch and verify nixexprs.tar.gz
multi sub MAIN(Bool:D :f(:$force) = False) {
  "Fetching “{nixexprs-file}” file…".say;

  if nixexprs-file-path.IO.f && !$force {
    "“{nixexprs-file-path}” file already exists, downloading is skipped.".say
  } else {
    my Str:D \release-link = release-link-file-path.IO.slurp.chomp;
    my Str:D \url = "{release-link}/{nixexprs-file}";
    "Downloading “{url}” and saving to “{nixexprs-file-path}” file…".say;
    run «curl --fail --output "{nixexprs-file-path}" -- "{url}"»;
  }

  verify-nixexprs-file-match-checksum;
  "SUCCESS".say
}

#| Upgrade “nixos” channel to the latest version
multi sub MAIN('upgrade') {
  'Running “upgrade”…'.say;

  "Resolving “{latest-channel-version}”…".say;

  my Str:D \release-link = run(«
    curl --fail -Ls -o /dev/null -w '%{url_effective}' -- "{latest-channel-version}"
  », :out).out.slurp(:close);

  "Resolved to “{release-link}”, saving to “{release-link-file-path}”…".say;
  mkdir relative unless relative.IO.d;
  spurt release-link-file-path, release-link;

  say
    "Trying to extract checksum for “{nixexprs-file}” file " ~
    "from HTML contents of “{release-link}” page…";

  my Str:D \nixexprs-checksum = extract-nixexprs-checksum release-link;
  "Found checksum “{nixexprs-checksum}”, saving to “{nixexprs-checksum-file-path}” file…".say;
  spurt nixexprs-checksum-file-path, nixexprs-checksum;

  "Extracting release date from HTML contents of “{release-link}” page…".say;
  my DateTime:D \release-date = extract-release-date release-link;
  "Got release date “{release-date}”, saving it to “{release-date-file-path}” file…".say;
  spurt release-date-file-path, release-date;

  for (git-revision-file, nixexprs-file) -> \file {
    my Str:D \url = "{release-link}/{file}";
    my Str:D \file-path = relative file;
    "Downloading “{url}” and saving to “{file-path}” file…".say;
    run «curl --fail --output "{file-path}" -- "{url}"»;
  }

  verify-nixexprs-file-match-checksum;
  "SUCCESS".say
}

constant \redefine-channels-commands = qq:to/COMMANDS/;
  sudo nix-channel --remove {channel-name}
  sudo nix-channel --add file:///etc/nixos/channels/{channel-name} {channel-name}
COMMANDS

sub USAGE {
  print $*USAGE ~ "\n\n" ~ qq:to/USAGE/;
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  By default it will:

  1. Download nixexprs.tar.gz archive
     (if it’s not downloaded already and you did’t set --force)

  2. Verify it matches the {nixexprs-checksum-file-path} checksum

  So after that you can redefine “nixos” channel like this:
  {redefine-channels-commands}
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  If you call “upgrade” subcommand it will:

  1. Resolve latest channel version link to a link of particular version/pick
     (will follow redirects). For example this:
     {latest-channel-version}
     Will resolve to a link that looks like this:
     https://releases.nixos.org/nixos/20.09/nixos-20.09alpha290.3b8ddb2f1ee

  2. The resolved link will be saved to
     {release-link-file-path} file

  3. From HTML document by that link SHA-256 checksum for {nixexprs-file} file
     will be extracted and saved to
     {nixexprs-checksum-file-path} file

  4. From the same HTML document release date will be extracted
     and saved to {release-date-file-path} file

  5. These two files will be downloaded:
       1. {git-revision-file}
       2. {nixexprs-file}
     and saved to {relative ''}

  6. {nixexprs-file-path} will be verified that it’s matching
     the {nixexprs-checksum-file-path} checksum

  USAGE
}
