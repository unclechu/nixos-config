# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
let sources = import ../../nix/sources.nix; in
{ callPackage
, writeScriptBin
, symlinkJoin
, makeWrapper
, lib

, rakudo
, xlibs # Just for ‘xinput’

# Overridable dependencies
, __nix-utils ? callPackage sources.nix-utils {}
}:
let
  inherit (__nix-utils) esc nameOfModuleFile shellCheckers;
  name = nameOfModuleFile (builtins.unsafeGetAttrPos "a" { a = 0; }).file;

  script = writeScriptBin name ''
    #! ${rakudo}/bin/raku
    use v6.d;
    close $*IN;

    my $device-name := 'Logitech G Pro';
    my @devices = run('xinput', 'list', '--short', :out).out.slurp(:close).chomp.lines;
    my $found-device-line = @devices.grep: / '↳ ' $device-name \s+ 'id='\d+ \s+ /;
    $found-device-line ~~ / 'id=' $<found-id> = (\d+) /;

    run('xinput', 'set-prop', $<found-id>, 'libinput Left Handed Enabled', '1');
  '';
in
symlinkJoin {
  inherit name;
  paths = [ script ];
  nativeBuildInputs = [ makeWrapper ];
  postBuild = ''
    wrapProgram "$out"/bin/${esc name} \
      --prefix PATH : ${esc (lib.makeBinPath [ xlibs.xinput ])}
  '';
}
