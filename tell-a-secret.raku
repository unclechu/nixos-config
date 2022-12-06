#! /usr/bin/env nix-shell
#! nix-shell --pure -i raku -E
#! nix-shell "let d=[p.rakudo p.coreutils p.gnupg p.diffutils];s=fetchTarball{url=\"https://releases.nixos.org/nixos/22.11/nixos-22.11.632.e169cf5b3b1/nixexprs.tar.xz\";sha256=\"187vx1ab3vj9xffy568w236yhb3gi8zwrhnb9km1bjwad32m5mq2\";};p=import s {};in p.mkShell{buildInputs=d;}"

# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

use v6.d;
$*PROGRAM.dirname.&*chdir;

my Str:D \hardware-dir = 'hardware';
my Str:D \keys-file = 'keys.secret';
my Str:D \decrypted-secret-postfix = '.secret.nix';
my Str:D \encrypted-secret-postfix = "{decrypted-secret-postfix}.asc";
my Str:D \machine-specific-secret-symlink = 'machine-specific.secret.nix';

# Apart from hardware secrets list that is formed automatically
my @extra-secrets is Array:D[Str:D] = keys-file, 'secret.nix';

sub expand-executable(Str:D \e) of Str:D {
  { given IO::Path.new: e, :CWD($_) { return .absolute if .e } } for $*SPEC.path;
  die error
    "‘{e}’ executable is not found! Paths used for searching:\n" ~
    $*SPEC.path.map({"  $_"}).join("\n")
}

# Guarding dependencies
constant \ls = expand-executable 'ls';
constant \ln = expand-executable 'ln';
constant \gpg = expand-executable 'gpg';
constant \diff = expand-executable 'diff';

# Shell text coloring helpers
sub red(Str:D \x --> Str:D) { "\e[31m{x}\e[0m" }
sub green(Str:D \x --> Str:D) { "\e[32m{x}\e[0m" }
sub blue(Str:D \x --> Str:D) { "\e[34m{x}\e[0m" }
sub yellow(Str:D \x --> Str:D) { "\e[1;33m{x}\e[0m" }

# More abstract shell text coloring helpers
sub error(Str:D \x --> Str:D) { red x }
sub warning(Str:D \x --> Str:D) { yellow x }
sub info(Str:D \x --> Str:D) { green x }
sub debug(Str:D \x --> Str:D) { blue x }

sub get-hardware-dir-files(--> Seq:D) {
  given run «"{ls}" -- "{hardware-dir}/"», :out { LEAVE {.sink}; .out.slurp(:close).lines }
}

# A list of fiels
sub get-hardware-secret-configs(Bool:D \encrypted, Str:D @hardware-dir-files, --> Seq:D) {
  my regex R { $(encrypted ?? encrypted-secret-postfix !! decrypted-secret-postfix) $ };
  @hardware-dir-files.grep(&R).map(*.subst(&R, ''))
}

sub validate-list-of-files(Array:D \given-list-files, Array:D \expected-list-of-files) {
  return if given-list-files ⊆ expected-list-of-files;

  die error
    "Provided list of files ({given-list-files.map({qq/‘$_’/}).join: ', '}) is not a subset of " ~
    "or equal to this list of expected files: {expected-list-of-files.map({qq/‘$_’/}).join: ', '}!"
}

#| Decrypt secret files and public keys used for encryption.
#| WARNING! This will override your changes in your previously decrypted files if you have some.
multi sub MAIN('decrypt', *@files) {
  my Str:D \hostname = '/etc/hostname'.IO.slurp(:close).chomp;
  my Str:D @hardware-dir-files = get-hardware-dir-files;
  my Str:D @hardware-configs-decrypted = get-hardware-secret-configs False, @hardware-dir-files;
  my Str:D @hardware-configs-encrypted = get-hardware-secret-configs True,  @hardware-dir-files;

  warn warning
    "Could not find associated encrypted machine-specific config for ‘{hostname}’ hostname. " ~
    'Maybe you didn’t rebuild your system yet? Build it without the decrypted secrets first ' ~
    'and then run this script, then you can rebuild the system again. Or maybe you just didn’t ' ~
    'create machine-specific secret config yet. In this case create ' ~
    "‘{hardware-dir}/{hostname}{decrypted-secret-postfix}’ " ~
    '(with at least this dummy-plug: ‘{...}: {}’) and run ‘encrypt’ command.'
    unless hostname ∈ @hardware-configs-encrypted;

  my Str:D @hardware-files =
    @hardware-configs-encrypted.map({ "{hardware-dir}/{$_}{decrypted-secret-postfix}" });

  my Str:D @files-to-decrypt = @extra-secrets.clone.append: @hardware-files;

  if @files.so {
    validate-list-of-files @files, @files-to-decrypt;
    @files-to-decrypt = @files;
  }

  for @files-to-decrypt {
    $*ERR.say: debug "\nHandling ‘$_’ file…";
    sub decrypt(Str:D \f) of Proc:D { run «"{gpg}" --decrypt --output "{f}" -- "{f}.asc"» }
    unless $_.IO ~~ :f { decrypt $_; next }

    my Str:D \encrypted-hash = do {
      my Proc:D \decrypt-proc = run «"{gpg}" --decrypt -- "$_.asc"», :out;
      given run "sha256sum", :in(decrypt-proc.out), :out {
        LEAVE { decrypt-proc.sink; .sink }
        .out.get.split(/\s+/)[0]
      }
    };

    my Str:D \decrypted-hash = do given run «sha256sum -- "{$_}"», :out {
      LEAVE { .sink }
      .out.get.split(/\s+/)[0]
    };

    if encrypted-hash eq decrypted-hash {
      $*ERR.say: debug
        "Decrypted ‘$_’ file already exists and matches the checksum of the contents of " ~
        'encrypted file. File is skipped.';
    } else {
      $*ERR.say: warning
        "Decrypted ‘$_’ file already exists and its content is different from encrypted file " ~
        "(‘$_.asc’). GPG will ask you to confirm overwriting the file. Be cautious! " ~
        'Maybe you made some changes in the file that you haven’t encrypted yet? ' ~
        'You may loose that data if you give a confirmation. ' ~
        "Run this command to see the difference between encrypted and unencrypted files:\n" ~
        "  '{$*PROGRAM}' diff '{$_}'";
      decrypt $_
    }
  }

  unless machine-specific-secret-symlink.IO ~~ :f {
    $*ERR.say: info "‘{machine-specific-secret-symlink}’ symlink does not exists, creating it…";

    run «
      "{ln}" -s --
      "{hardware-dir}/{hostname}{decrypted-secret-postfix}"
      "{machine-specific-secret-symlink}"
    »;
  }
}

#| Encrypt your changes in secret files thus you can commit them and push to the repo.
multi sub MAIN('encrypt', Bool:D :f(:$force) = False, *@files) {
  die error "‘{keys-file}’ does not exists. Did you run ‘decrypt’ command first?"
    unless keys-file.IO ~~ :f;

  my Array:D \recipients =
    keys-file.IO.slurp(:close).lines
      .Array
      .prepend($[False, []])
      .reduce(sub (Array:D \acc, Str:D \x --> Array:D) {
        if     acc[0] && x eq '-----END PGP PUBLIC KEY BLOCK-----'   { acc[0] = False   }
        elsif !acc[0] && x eq '-----BEGIN PGP PUBLIC KEY BLOCK-----' { acc[0] = True    }
        elsif !acc[0] && x.so                                        { acc[1].append: x }
        acc
      })
      .pop;

  my Str:D @hardware-dir-files = get-hardware-dir-files;

  my Str:D @hardware-configs-decrypted =
    get-hardware-secret-configs(False, @hardware-dir-files)
      .map({ "{hardware-dir}/{$_}{decrypted-secret-postfix}" });

  my Str:D @files-to-encrypt = @extra-secrets.clone.append: @hardware-configs-decrypted;

  if @files.so {
    validate-list-of-files @files, @files-to-encrypt;
    @files-to-encrypt = @files;
  }

  die error "Secret file ‘{$_}’ not found! Did you run ‘decrypt’ command first?"
    unless .IO ~~ :f for @files-to-encrypt;

  for @files-to-encrypt {
    $*ERR.say: debug "\nHandling ‘$_’ file…";

    # If *.asc file doesn’t exist it means it’s a new file to be encrypted for the first time
    if !$force && "$_.asc".IO ~~ :e {
      my Str:D \encrypted-hash = do {
        my Proc:D \decrypt-proc = run «"{gpg}" --decrypt -- "$_.asc"», :out;
        given run "sha256sum", :in(decrypt-proc.out), :out {
          LEAVE { decrypt-proc.sink; .sink }
          .out.get.split(/\s+/)[0]
        }
      };

      my Str:D \decrypted-hash = do given run «sha256sum -- "{$_}"», :out {
        LEAVE { .sink }
        .out.get.split(/\s+/)[0]
      };

      if encrypted-hash eq decrypted-hash {
        $*ERR.say: debug
          "The checksum of ‘$_’ file and checksum of content of encrypted ‘$_.asc’ file match. " ~
          'There are no changes to encrypt. If you still want to re-encrypt the file then just ' ~
          "call the command with ‘--force’ flag. ‘$_’ file is skipped.";
        next
      }
    } elsif !$force && "$_.asc".IO !~~ :e {
      $*ERR.say: info "‘$_’ seems to be a new file. Encrypting it for the first time…"
    }

    my Array:D \recipients-arguments =
      recipients
        .Array
        .prepend($[])
        .reduce(sub (Array:D \acc, Str:D \x --> Array:D) { acc.append: '--recipient', x });

    my Str:D @args = (
      «"{gpg}" --armor --encrypt»,
      recipients-arguments,
      «--output "{$_}.asc" -- "$_"»
    ).flat;

    $*ERR.say: warning
      "Decrypted ‘$_’ file is different from its encrypted counterpart (‘$_.asc’). " ~
      'GPG will ask you to confirm overwriting the encrypted file. Be cautious! ' ~
      'In case you have new changes in both you can loose some data from the encrypted file. ' ~
      "Run this command to see the difference between encrypted and unencrypted files:\n" ~
      "  '{$*PROGRAM}' diff '{$_}'";

    $*ERR.say: debug
      @args.map({ /^<[a..zA..Z0..9.\-_]>+$/ ?? $_ !! "'$_'" }).Array.prepend('+').join(' ');

    run @args
  }
}

#| Import public keys to use them for encryption.
multi sub MAIN('import') {
  die error "‘{keys-file}’ does not exists. Did you run ‘decrypt’ command first?"
    unless keys-file.IO ~~ :f;

  run «"{gpg}" --import -- "{keys-file}"»
}

#| Show the difference between encrypted and unencrypted files.
#| First goes encrypted source and the second is its unencrypted counterpart.
multi sub MAIN('diff', *@files) {
  my Str:D \hostname = '/etc/hostname'.IO.slurp(:close).chomp;
  my Str:D @hardware-dir-files = get-hardware-dir-files;
  my Str:D @hardware-configs-decrypted = get-hardware-secret-configs False, @hardware-dir-files;
  my Str:D @hardware-configs-encrypted = get-hardware-secret-configs True,  @hardware-dir-files;

  my Str:D @hardware-files =
    @hardware-configs-encrypted.map({ "{hardware-dir}/{$_}{decrypted-secret-postfix}" });

  my Str:D @files-to-diff = @extra-secrets.clone.append: @hardware-files;

  if @files.so {
    # Validate existence of the encrypted source files
    validate-list-of-files @files, @files-to-diff;
    @files-to-diff = @files;
  }

  # Validate existence of unencrypted counterparts that we are diff-ing
  die error "Secret file ‘{$_}’ not found! Did you run ‘decrypt’ command first?"
    unless .IO ~~ :f for @files-to-diff;

  for @files-to-diff {
    $*ERR.say: debug "\nHandling ‘$_’ file…";

    # Decrypted contents of the encrypted file.
    # NOT the contents of the decrypted file.
    my Str:D \decrypted-contents = do
      given run «"{gpg}" --decrypt -- "$_.asc"», :out { LEAVE {.sink}; .out.slurp(:close) };

    # Hash of the decrypted contents of the encrypted file
    my Str:D \encrypted-hash = do given run "sha256sum", :in, :out {
      LEAVE { .sink }
      .in.spurt(decrypted-contents, :close);
      .out.get.split(/\s+/)[0];
    };

    # Hash of the decrypted file
    my Str:D \decrypted-hash = do given run «sha256sum -- "{$_}"», :out {
      LEAVE { .sink }
      .out.get.split(/\s+/)[0]
    };

    if encrypted-hash eq decrypted-hash {
      $*ERR.say: debug
        "The checksum of ‘$_’ file and checksum of content of encrypted ‘$_.asc’ file match. " ~
        'There is no difference between the two.';
      next
    }

    $*ERR.say: info
      "The difference between ’$_.asc’ and its unencrypted counterpart ’$_‘:";

    given run «"{diff}" --color - -- "$_"», :in {
      LEAVE { .so } # Ignore non-zero exit code by using “.so”
      .in.spurt(decrypted-contents, :close);
    }
  }
}
