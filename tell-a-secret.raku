#! /usr/bin/env nix-shell
#! nix-shell --pure -i raku -E
#! nix-shell "let d=[p.rakudo p.coreutils p.gnupg];s=fetchTarball{url=\"https://releases.nixos.org/nixos/21.05/nixos-21.05.1523.b2f87e0043a/nixexprs.tar.xz\";sha256=\"19py1c61b4sxy2x4qy6m6k75jih1qnlf0a50klrkv384fqxv02v9\";};p=import s {};in p.mkShell{buildInputs=d;}"

# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

use v6.d;
$*PROGRAM.dirname.&*chdir;

my Str:D \hardware-dir = 'hardware';
my Str:D \keys-file = 'keys.secret';
my Str:D \decrypted-secret-postfix = '.secret.nix';
my Str:D \encrypted-secret-postfix = "{decrypted-secret-postfix}.asc";
my Str:D \machine-specific-secret-symlink = 'machine-specific.secret.nix';

sub expand-executable(Str:D \e) of Str:D {
  { given IO::Path.new: e, :CWD($_) { return .absolute if .e } } for $*SPEC.path;
  die
    "‘{e}’ executable is not found! Paths used for searching:\n" ~
    $*SPEC.path.map({"  $_"}).join("\n")
}

# Guarding dependencies
constant \ls  = expand-executable 'ls';
constant \ln  = expand-executable 'ln';
constant \gpg = expand-executable 'gpg';

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

  die
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

  warn
    "Could not find associated encrypted machine-specific config for ‘{hostname}’ hostname. " ~
    'Maybe you didn’t rebuild your system yet? Build it without the decrypted secrets first ' ~
    'and then run this script, then your can rebuild the system again. Or maybe you just didn’t ' ~
    'create machine-specific secret config yet. In this case create ' ~
    "‘{hardware-dir}/{hostname}{decrypted-secret-postfix}’ " ~
    '(with at least this dummy-plug: ‘{...}: {}’) and run ‘encrypt’ command.'
    unless hostname ∈ @hardware-configs-encrypted;

  my Str:D @hardware-files =
    @hardware-configs-encrypted.map({ "{hardware-dir}/{$_}{decrypted-secret-postfix}" });

  my Str:D @files-to-decrypt = «keys.secret secret.nix».Array.append: @hardware-files;

  if @files.so {
    validate-list-of-files @files, @files-to-decrypt;
    @files-to-decrypt = @files;
  }

  run «"{gpg}" --decrypt --output "$_" -- "$_.asc"» for @files-to-decrypt;

  unless machine-specific-secret-symlink.IO ~~ :f {
    $*ERR.say: "‘{machine-specific-secret-symlink}’ symlink does not exists, creating it…";

    run «
      "{ln}" -s --
      "{hardware-dir}/{hostname}{decrypted-secret-postfix}"
      "{machine-specific-secret-symlink}"
    »;
  }
}

#| Encrypt your changes in secret files thus you can commit them and push to the repo.
multi sub MAIN('encrypt', *@files) {
  die "‘{keys-file}’ does not exists. Did you run ‘decrypt’ command first?"
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

  my Str:D @files-to-encrypt = «keys.secret secret.nix».Array.append: @hardware-configs-decrypted;

  if @files.so {
    validate-list-of-files @files, @files-to-encrypt;
    @files-to-encrypt = @files;
  }

  die "Secret file ‘{$_}’ not found! Did you run ‘decrypt’ command first?"
    unless .IO ~~ :f for @files-to-encrypt;

  for @files-to-encrypt {
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

    @args.flat.map({ /^<[a..zA..Z0..9.\-_]>+$/ ?? $_ !! "'$_'" }).Array.prepend('+').join(' ').say;
    run @args
  }
}

#| Import public keys to use them for encryption.
multi sub MAIN('import') {
  die "‘{keys-file}’ does not exists. Did you run ‘decrypt’ command first?"
    unless keys-file.IO ~~ :f;

  run «"{gpg}" --import -- "{keys-file}"»
}
