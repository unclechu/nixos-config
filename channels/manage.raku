#! /usr/bin/env nix-shell
#! nix-shell -i raku -E
#! nix-shell "let d=[p.rakudo p.coreutils p.curl p.cacert p.libxml2];c=\"3b8ddb2f1ee6f4c0794fb6dfbd273c3599492b76\";h=\"06hgvyd8ry4i49dmjxh5n6wv1j5ifpp7i3a7bjz62san0q6d0j35\";s=fetchTarball{url=\"https://github.com/NixOS/nixpkgs/archive/${c}.tar.gz\";sha256=h;};p=import s {};in p.mkShell{buildInputs=d;}"
#↑ The pick from above is taken from branch nixos-20.09
use v6.d;
$*PROGRAM.dirname.&*chdir;

constant %channels =
  'nixos'          => 'nixos-20.09',
  'nixos-unstable' => 'nixos-unstable';

constant \channel-path-prefix = 'file:///etc/nixos/channels/';

constant \channel-url-prefix = 'https://nixos.org/channels/';
sub channel-url(Str:D \channel-name) of Str:D { channel-url-prefix ~ channel-name }

constant \nixexprs-file-name          = 'nixexprs.tar.xz';
constant \nixexprs-checksum-file-name = 'nixexprs-sha256-checksum';
constant \git-revision-file-name      = 'git-revision';
constant \release-link-file-name      = 'release-link';
constant \release-date-file-name      = 'release-date';

sub channel-file(Str:D \channel-name, *@sub-path) of Str:D {
  ($*CWD, channel-name, |@sub-path).join: '/'
}

sub extract-release-date(Str:D \html) of DateTime:D {
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

sub extract-nixexprs-checksum(Str:D \html) of Str:D {
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
        return @lines[1] if @lines[0] eq nixexprs-file-name
      }
    } || Nil
  }();

  fail "SHA-256 checksum of “{nixexprs-file-name}” file was not found in provided HTML!"
    unless found-hash.so;

  found-hash
}

sub verify-nixexprs-file-match-checksum(Str:D \channel-name) {
  my Str:D \nixexprs-file-path          = channel-file channel-name, nixexprs-file-name;
  my Str:D \nixexprs-checksum-file-path = channel-file channel-name, nixexprs-checksum-file-name;
  my Str:D \checksum                    = nixexprs-checksum-file-path.IO.slurp.chomp;

  "Verifying that “{nixexprs-file-path}” is matching “{checksum}” checksum…".say;

  my Str:D \actual-checksum =
    run(«sha256sum -- "{nixexprs-file-path}"», :out).out.slurp(:close).chomp.split(/\s+/)[0];

  fail
    "Actual checksum of “{nixexprs-file-path}” which is “{actual-checksum}” " ~
    "doesn’t match “{checksum}” checksum from “{nixexprs-checksum-file-path}” file!"
      if actual-checksum ne checksum
}

sub log-channels(*@channel-names) of Str:D { @channel-names.map('“'~*~'”').join: ', ' }

sub default-channel-names(Array:D $channel-names) of Seq:D {
  return %channels.keys.sort unless $channel-names.elems > 0;
  given reduce -> @x, $k { %channels{$k}:exists ?? @x !! (|@x, $k) }, (), |$channel-names {
    die 'Unknown channel name(s): ' ~ log-channels @_ if @_.elems > 0
  }
  $channel-names.unique.Seq
}

#| Fetch and verify nixexprs file
multi sub MAIN('fetch', Bool:D :f(:$force) = False, *@channel-names) {
  @channel-names = default-channel-names @channel-names;
  "Fetching “{nixexprs-file-name}” file for these channels: {log-channels @channel-names}…".say;

  for @channel-names -> \channel-name {
    "Fetching “{nixexprs-file-name}” for “{channel-name}” channel…".say;
    my Str:D \nixexprs-file-path = channel-file channel-name, nixexprs-file-name;

    if nixexprs-file-path.IO.f && !$force {
      "“{nixexprs-file-path}” file already exists, downloading is skipped.".say
    } else {
      my Str:D \release-link = (channel-file channel-name, release-link-file-name).IO.slurp.chomp;
      my Str:D \url = "{release-link}/{nixexprs-file-name}";
      "Downloading “{url}” and saving to “{nixexprs-file-path}” file…".say;
      run «curl --fail --output "{nixexprs-file-path}" -- "{url}"»;
    }

    verify-nixexprs-file-match-checksum channel-name;
    "SUCCESS (for “{channel-name}” channel)".say
  }

  "SUCCESS".say
}

#| Upgrade channel(s) to the latest version
multi sub MAIN('upgrade', *@channel-names) {
  @channel-names = default-channel-names @channel-names;
  "Upgrading these channels: {log-channels @channel-names}…".say;

  for @channel-names -> \channel-name {
    "Upgrading “{channel-name}” channel…".say;
    my Str:D \channel-latest-version-url = channel-url %channels{channel-name};
    "Resolving “{channel-latest-version-url}”…".say;

    my Str:D \release-link = run(«
      curl --fail -Ls -o /dev/null -w '%{url_effective}' -- "{channel-latest-version-url}"
    », :out).out.slurp(:close);

    my Str:D \release-link-file-path = channel-file channel-name, release-link-file-name;
    "Resolved to “{release-link}”, saving to “{release-link-file-path}”…".say;
    mkdir channel-file channel-name unless (channel-file channel-name).IO.d;
    spurt release-link-file-path, release-link;

    say
      "Trying to extract checksum for “{nixexprs-file-name}” file " ~
      "from HTML contents of “{release-link}” page…";

    my Str:D \release-html = run(«
      curl --fail --no-progress-meter -- "{release-link}"
    », :out).out.slurp(:close);

    my Str:D \nixexprs-checksum = extract-nixexprs-checksum release-html;
    my Str:D \nixexprs-checksum-file-path = channel-file channel-name, nixexprs-checksum-file-name;
    "Found checksum “{nixexprs-checksum}”, saving to “{nixexprs-checksum-file-path}” file…".say;
    spurt nixexprs-checksum-file-path, nixexprs-checksum;

    "Extracting release date from HTML contents of “{release-link}” page…".say;
    my DateTime:D \release-date = extract-release-date release-html;
    my Str:D \release-date-file-path = channel-file channel-name, release-date-file-name;
    "Got release date “{release-date}”, saving it to “{release-date-file-path}” file…".say;
    spurt release-date-file-path, release-date;

    for (git-revision-file-name, nixexprs-file-name) -> \file {
      my Str:D \url = "{release-link}/{file}";
      my Str:D \file-path = channel-file channel-name, file;
      "Downloading “{url}” and saving to “{file-path}” file…".say;
      run «curl --fail --output "{file-path}" -- "{url}"»;
    }

    verify-nixexprs-file-match-checksum channel-name;
    "SUCCESS (for “{channel-name}” channel)".say
  }

  "SUCCESS".say
}

#| Override channel(s) to ones managed by this script (requires “sudo” access)
multi sub MAIN('override', *@channel-names) {
  @channel-names = default-channel-names @channel-names;
  "Overriding these channels: {log-channels @channel-names}…".say;
  "Requesting current system channels list (you might be asked for “sudo” password)…".say;

  my Str:D %current-channels =
    run(<sudo nix-channel --list>, :out).out.slurp(:close).chomp.lines.map(*.split: /\s+/).flat;

  for @channel-names -> \channel-name {
    "Overriding “{channel-name}” channel…".say;
    my Str:D \path = channel-path-prefix ~ channel-name;

    if %current-channels{channel-name}:exists {
      if %current-channels{channel-name} eq path {
        "“{channel-name}” channel already exists and already points to “{path}”, skipping…".say;
        "SUCCESS (for “{channel-name}” channel)".say;
        next;
      }

      say
        "“{channel-name}” channel already exists but it points to " ~
        "“{%current-channels{channel-name}}” whilst we need it to point to “{path}”, removing it…";

      run «sudo nix-channel --remove -- "{channel-name}"»;
    } else {
      "“{channel-name}” doesn’t exists (it’s okay).".say;
    }

    "Adding new channel “{channel-name}” and pointing it to “{path}”…".say;
    run «sudo nix-channel --add -- "{path}" "{channel-name}"»;
    "SUCCESS (for “{channel-name}” channel)".say
  }

  "SUCCESS".say
}

sub USAGE {
  constant \nixos-channel = %channels.keys.sort[0];
  sub file(*@sub-path) of Str:D { ($*CWD, '%CHANNEL_NAME%', |@sub-path).join: '/' }

  print $*USAGE ~ "\n\n" ~ qq:to/USAGE/;
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  Channels are:

  {%channels.kv.sort.map(-> $k, $v {"• $k → {channel-url $v}"}).join: "\n"}

  If you call “fetch” subcommand it will
  (for all the channels if you didn’t specify particular one):

  1. Download “{nixexprs-file-name}” archive
     (if it’s not downloaded already and you did’t set “--force”)

  2. Verify that “{file nixexprs-file-name}”
     archive matches checksum from
     “{file nixexprs-checksum-file-name}” file

  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  If you call “upgrade” subcommand it will
  (for all the channels if you didn’t specify particular one):

  1. Resolve latest channel version link to a link of particular version/pick
     (will follow redirects). For example this:
     {channel-url nixos-channel}
     Will resolve to a link that looks like this:
     https://releases.nixos.org/nixos/20.09/nixos-20.09alpha290.3b8ddb2f1ee

  2. The resolved link will be saved to
     “{file release-link-file-name}” file

  3. From HTML document by that link SHA-256 checksum
     for “{nixexprs-file-name}” file will be extracted and saved to
     “{file nixexprs-checksum-file-name}” file

  4. From the same HTML document release date will be extracted
     and saved to “{file release-date-file-name}” file

  5. These two files will be downloaded:

     1. {git-revision-file-name}
     2. {nixexprs-file-name}

     and saved to {file ''}

  6. “{file nixexprs-file-name}”
     file will be verified that it’s matching checksum from
     “{file nixexprs-checksum-file-name}” file

  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  If you call “override” subcommand it will
  (for all the channels if you didn’t specify particular one):

  1. Check if a channel with the same name is already present in the system
     and it will be either skipped if it already points to the proper channel
     directory or it will be removed from the channels list

  2. New channel will be added to the system pointing to local directory

  The “sudo nix-channel --list | column -t” would look like this after:
    nixos           file:///etc/nixos/channels/nixos
    nixos-unstable  file:///etc/nixos/channels/nixos-unstable

  USAGE
}