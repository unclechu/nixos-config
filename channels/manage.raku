#! /usr/bin/env nix-shell
#! nix-shell --pure -i raku -E
#! nix-shell "let d=[p.rakudo p.coreutils p.curl p.cacert p.libxml2 p.nix];s=fetchTarball{url=\"https://releases.nixos.org/nixos/23.05/nixos-23.05.1875.8df7a67abaf/nixexprs.tar.xz\";sha256=\"1lvg148yng2mxy1mzfzc3xbzyksh6rkh8wbh159qz88kl4rx0n4p\";};p=import s {};in p.mkShell{buildInputs=d;}"

# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

use v6.d;
$*PROGRAM.dirname.&*chdir;

constant %channels =
  'nixos'          => 'nixos-23.05',
  'nixos-unstable' => 'nixos-unstable';

my Str:D \channel-path-prefix = "file://$*CWD/";

constant \channel-url-prefix = 'https://nixos.org/channels/';
sub channel-url(Str:D \channel-name) of Str:D { channel-url-prefix ~ channel-name }

constant \nixexprs-file-name                   = 'nixexprs.tar.xz';
constant \nixexprs-checksum-file-name          = 'nixexprs-sha256-checksum';
constant \nixexprs-unpacked-checksum-file-name = 'nixexprs-unpacked-sha256-checksum';
constant \git-revision-file-name               = 'git-revision';
constant \release-link-file-name               = 'release-link';
constant \release-date-file-name               = 'release-date';

my IO::Path:D \channels-manage-script-path = $*PROGRAM;
my IO::Path:D \tell-a-secret-script-path   = $*CWD.add('..').add('tell-a-secret.raku').resolve;

sub expand-executable(Str:D \e) of Str:D {
  { given IO::Path.new: e, :CWD($_) { return .absolute if .e } } for $*SPEC.path;
  die
    "‘{e}’ executable is not found! Paths used for searching:\n" ~
    $*SPEC.path.map({"  $_"}).join("\n")
}

# Guarding dependencies
constant \sudo             = expand-executable '/run/wrappers/bin/sudo';
constant \xmllint          = expand-executable 'xmllint';
constant \sha256sum        = expand-executable 'sha256sum';
constant \curl             = expand-executable 'curl';
constant \nix-channel      = expand-executable 'nix-channel';
constant \nix-prefetch-url = expand-executable 'nix-prefetch-url';

sub channel-file(Str:D \channel-name, *@sub-path) of Str:D {
  ($*CWD, channel-name, |@sub-path).join: '/'
}

sub extract-release-date(Str:D \html) of DateTime:D {
  given run «"{xmllint}" --format --html --xpath '//html/body/p/child::text()' -», :in, :out {
    LEAVE { .sink }
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
  my Str:D @trs = do given run «
    "{xmllint}" --format --html --xpath //html/body/table/tbody/tr -
  », :in, :out {
    LEAVE { .sink }
    .in.print: html;
    .in.close;
    .out.slurp(:close).chomp.lines
  };

  my \found-hash = sub {
    do for @trs -> $tr {
      given run «
        "{xmllint}" --format --html --xpath '//tr/td/*/child::node()' -
      », :in, :out {
        LEAVE { .sink }
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

class FileChecksumMismatch is Exception {
  has Str $.expected-checksum           is readonly;
  has Str $.expected-checksum-file-path is readonly;
  has Str $.file-path                   is readonly;
  has Str $.actual-checksum             is readonly;

  method message() {
    "Actual checksum of “{$!file-path}” which is “{$!actual-checksum}” doesn’t match " ~
    "expected “{$!expected-checksum}” checksum from “{$!expected-checksum-file-path}” file!"
  }
}

sub verify-nixexprs-file-matches-checksum(Str:D \channel-name) {
  my Str:D \nixexprs-file-path          = channel-file channel-name, nixexprs-file-name;
  my Str:D \nixexprs-checksum-file-path = channel-file channel-name, nixexprs-checksum-file-name;
  my Str:D \checksum                    = nixexprs-checksum-file-path.IO.slurp.chomp;

  "Verifying that “{nixexprs-file-path}” is matching “{checksum}” checksum…".say;

  my Str:D \actual-checksum =
    do given run «"{sha256sum}" -- "{nixexprs-file-path}"», :out {
      LEAVE { .sink }
      .out.slurp(:close).chomp.split(/\s+/)[0]
    };

  FileChecksumMismatch.new(
    expected-checksum           => checksum,
    expected-checksum-file-path => nixexprs-checksum-file-path,
    file-path                   => nixexprs-file-path,
    actual-checksum             => actual-checksum,
  ).throw if actual-checksum ne checksum
}

# Prefetches “nixexprs” file and returns SHA-256 checksum of it’s unpacked version that can be used
# with “fetchTarball”.
sub prefetch-nixpkgs-checksum(
  Str:D \channel-name,
  Str:D \release-link,
  Bool:D :$just-prefetch = False
) of Str:D {
  my Str:D \nixexprs-file-path = channel-file channel-name, nixexprs-file-name;
  my Str:D \nixexprs-file-url  = "{channel-path-prefix}{channel-name}/{nixexprs-file-name}";
  my Str:D \checksum-file-path = channel-file channel-name, nixexprs-unpacked-checksum-file-name;
  my Str:D \nixexprs-release-url = "{release-link}/{nixexprs-file-name}";

  "Prefetching “{nixexprs-file-path}” file and getting its unpacked hash…".say;
  die "Could not find “{nixexprs-file-path}” file to prefetch it!" unless nixexprs-file-path.IO.f;

  sub fetch(Str:D \url) of Str:D {
    given run «"{nix-prefetch-url}" --unpack -- "{url}"», :out {
      LEAVE { .sink }
      .out.slurp(:close).chomp
    }
  }

  my Str:D \checksum-from-local-file = fetch nixexprs-file-url;

  say
    "Also prefetching this URL “{nixexprs-release-url}” " ~
    "(it’s going to be used as a “nixpkgs” pin in scripts)…";

  my Str:D \checksum-from-release-link = fetch nixexprs-release-url;

  "Verifying that checksums of both prefetches are matching…".say;

  fail
    "Prefetched checksum for “{nixexprs-file-url}” (“{checksum-from-local-file}”) " ~
    "does not match with one for “{nixexprs-release-url}” (“{checksum-from-release-link}”)!"
    unless checksum-from-local-file eq checksum-from-release-link;

  unless $just-prefetch {
    "Saving “{checksum-from-local-file}” hash to “{checksum-file-path}” file…".say;
    spurt checksum-file-path, checksum-from-local-file;
  }

  checksum-from-local-file
}

sub patch-nix-shell-script(IO::Path:D \script-path, Str:D \url, Str:D \hash) {
  say
    "Patching “{script-path.absolute}” script file " ~
    "(updating “nixpkgs” pin to “{url}” URL and “{hash}” SHA-256 hash)…";

  given script-path.absolute {
    my Seq:D \lines = .IO.slurp.lines(:chomp(False)).map({
      if / ^\#\! \s nix\-shell .+ fetch .+ url \s* \= .+ sha256 \s* \= / {
        my Str:D $str = $_;
        $str ~~ s/(url    \s* \= \s* \\\") \S+? (\\\")/{$0}{url }{$1}/;
        $str ~~ s/(sha256 \s* \= \s* \\\") \S+? (\\\")/{$0}{hash}{$1}/;
        $str
      } else {
        $_
      }
    });

    spurt $_, lines.join: ''
  }
}

sub update-channels(*@channel-names) {
  return unless @channel-names.elems > 0; # nothing to do

  say
    "Updating these channels: {log-channels @channel-names} " ~
    "(via “sudo”, you may be asked for password)…";

  run «"{sudo}" "{nix-channel}" --update --», @channel-names;
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

  my Str:D @channel-names-to-update = ();

  for @channel-names -> \channel-name {
    "Fetching “{nixexprs-file-name}” for “{channel-name}” channel…".say;
    my Str:D \nixexprs-file-path = channel-file channel-name, nixexprs-file-name;

    sub download {
      my Str:D \release-link = (channel-file channel-name, release-link-file-name).IO.slurp.chomp;
      my Str:D \url = "{release-link}/{nixexprs-file-name}";

      "Downloading “{url}” and saving to “{nixexprs-file-path}” file…".say;
      run «"{curl}" --fail --output "{nixexprs-file-path}" -- "{url}"»;

      verify-nixexprs-file-matches-checksum channel-name;
      prefetch-nixpkgs-checksum channel-name, release-link, :just-prefetch;

      "Adding channel “{channel-name}” to the list of channels to update…".say;
      @channel-names-to-update.push: channel-name;
    }

    if nixexprs-file-path.IO.f && !$force {
      say
        "“{nixexprs-file-path}” file already exists, checking that it matches its checksum " ~
        'in order to check whether we need to download a different version of it ' ~
        '(it usually happens when you update a pin and try to “fetch” on an another machine ' ~
        'where it’s still an older version previously downloaded)…';

      try verify-nixexprs-file-matches-checksum channel-name;
      given $! {
        when FileChecksumMismatch {
          "“{nixexprs-file-name}” is outdated, downloading it…".say;
          download
        }
        when .so { .throw } # Rethrow unexpected exception
        default { "“{nixexprs-file-name}” is up to date.".say }
      }
    } else {
      download
    }

    "SUCCESS (for “{channel-name}” channel)".say
  }

  update-channels @channel-names-to-update;
  "SUCCESS".say
}

#| Upgrade channel(s) to the latest version
multi sub MAIN('upgrade', Bool:D :f(:$force) = False, *@channel-names) {
  @channel-names = default-channel-names @channel-names;
  "Upgrading these channels: {log-channels @channel-names}…".say;

  my Str:D @channel-names-to-update = ();

  for @channel-names -> \channel-name {
    "Upgrading “{channel-name}” channel…".say;
    my Str:D \channel-latest-version-url = channel-url %channels{channel-name};
    "Resolving “{channel-latest-version-url}”…".say;

    my Str:D \release-link =
      do given run «
        "{curl}" --fail -Ls -o /dev/null -w '%{url_effective}' --
        "{channel-latest-version-url}"
      », :out {
        LEAVE { .sink }
        .out.slurp(:close)
      };

    my Str:D \release-link-file-path = channel-file channel-name, release-link-file-name;
    "Resolved to “{release-link}”.".say;
    mkdir channel-file channel-name unless (channel-file channel-name).IO.d;

    if !$force {
      "Checking whether an upgrade is needed…".say;

      if (my \file = release-link-file-path.IO).f && file.slurp.chomp eq release-link {
        "“{channel-name}” is already latest version, nothing to upgrade.".say;
        "SUCCESS (for “{channel-name}” channel)".say;
        next
      }
    }

    "Saving “{release-link}” to “{release-link-file-path}” file…".say;
    spurt release-link-file-path, release-link;

    say
      "Trying to extract checksum for “{nixexprs-file-name}” file " ~
      "from HTML contents of “{release-link}” page…";

    my Str:D \release-html =
      do given run «"{curl}" --fail --no-progress-meter -- "{release-link}"», :out {
        LEAVE { .sink }
        .out.slurp(:close)
      };

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
      run «"{curl}" --fail --output "{file-path}" -- "{url}"»;
    }

    verify-nixexprs-file-matches-checksum channel-name;
    my Str:D \unpacked-checksum = prefetch-nixpkgs-checksum channel-name, release-link;

    if channel-name eq 'nixos' {
      patch-nix-shell-script $_, "{release-link}/{nixexprs-file-name}", unpacked-checksum
        for (channels-manage-script-path, tell-a-secret-script-path)
    }

    "Adding channel “{channel-name}” to the list of channels to update…".say;
    @channel-names-to-update.push: channel-name;

    "SUCCESS (for “{channel-name}” channel)".say
  }

  update-channels @channel-names-to-update;
  "SUCCESS".say
}

#| Override channel(s) to ones managed by this script (requires “sudo” access)
multi sub MAIN('override', *@channel-names) {
  @channel-names = default-channel-names @channel-names;
  "Overriding these channels: {log-channels @channel-names}…".say;
  "Requesting current system channels list (you might be asked for “sudo” password)…".say;

  my Str:D @channel-names-to-update = ();

  my Str:D %current-channels =
    do given run «"{sudo}" "{nix-channel}" --list», :out {
      LEAVE { .sink }
      .out.slurp(:close).chomp.lines.map(*.split: /\s+/).flat
    };

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

      run «"{sudo}" "{nix-channel}" --remove -- "{channel-name}"»;
    } else {
      "“{channel-name}” doesn’t exists (it’s okay).".say;
    }

    "Adding new channel “{channel-name}” and pointing it to “{path}”…".say;
    run «"{sudo}" "{nix-channel}" --add -- "{path}" "{channel-name}"»;

    "Adding channel “{channel-name}” to the list of channels to update…".say;
    @channel-names-to-update.push: channel-name;

    "SUCCESS (for “{channel-name}” channel)".say
  }

  update-channels @channel-names-to-update;
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

  3. Prefetch “{nixexprs-file-name}” both from
     local file system and from release URL.

  4. Update the channels that were being fetched
     (via “sudo”, you may be asked for password)

  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  If you call “upgrade” subcommand it will
  (for all the channels if you didn’t specify particular one):

  1.  Resolve latest channel version link to a link of particular version/pin
      (will follow redirects). For example this:
      {channel-url nixos-channel}
      Will resolve to a link that looks like this:
      https://releases.nixos.org/nixos/20.09/nixos-20.09alpha290.3b8ddb2f1ee

  2.  Check whether an upgrade is needed by comparing link from
      “{file release-link-file-name}” file
      with the resolved link (if you did’t set “--force” of course)

  3.  The resolved link will be saved to
      “{file release-link-file-name}” file

  4.  From HTML document by that link SHA-256 checksum
      for “{nixexprs-file-name}” file will be extracted and saved to
      “{file nixexprs-checksum-file-name}” file

  5.  From the same HTML document release date will be extracted
      and saved to “{file release-date-file-name}” file

  6.  These two files will be downloaded:

      1. {git-revision-file-name}
      2. {nixexprs-file-name}

      and saved to “{file ''}”

  7.  “{file nixexprs-file-name}”
      file will be verified that it’s matching checksum from
      “{file nixexprs-checksum-file-name}” file

  8.  “{nixexprs-file-name}” file will be prefetched with “--unpack” flag.
      It will be prefecthed both from local file system and from the release URL.
      Prefetched “unpacked” checksum will be checked that it matches for
      both prefetches. And then it will be saved into
      “{file nixexprs-unpacked-checksum-file-name}” file.

  9.  New release URL for “{nixexprs-file-name}” file and its prefetched
      checksum (from “nixos” channel) will be used to update the “nixpkgs”
      pin in this script (“{channels-manage-script-path.resolve}”)
      and also in this one: “{tell-a-secret-script-path}”

  10. Channels that were “upgraded” will be updated
      (via “sudo”, you may be asked for password).

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
