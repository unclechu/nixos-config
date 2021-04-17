# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

my $device-name := 'Logitech Rechargeable Touchpad T650';
my @devices = run(xinput, 'list', '--short', :out).out.slurp(:close).chomp.lines;
my $found-device-line = @devices.grep: / '↳ ' $device-name \s+ 'id='\d+ \s+ /;
$found-device-line ~~ / 'id=' $<found-id> = (\d+) /;

run(xinput, 'set-prop', $<found-id>, 'libinput Natural Scrolling Enabled', '1');
