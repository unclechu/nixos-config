#! /usr/bin/env perl
# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

use v5.10; use strict; use warnings;
use List::Util qw<first>;
use Getopt::Long;
use Pod::Usage;

use IPC::Cmd qw(can_run);
sub need_exe { my ($cmd) = @_; can_run($cmd) or die "$cmd: executable dependency not found\n"; }

BEGIN { # Guard dependencies
	need_exe 'xrandr';
  need_exe 'xdotool';
}

my $cursor_place = 'rb'; # (r)ight-(b)ottom
my $show_help = 0;
my $display;
my @cursor_places = ('lt', 'ct', 'rt', 'lc', 'cc', 'rc', 'lb', 'cb', 'rb');
my %pos_map = (l => 10, t => 10, r => 90, b => 90, c => 50);

sub fail_usage { pod2usage(-exitval => 1, -verbose => 1) }

sub safe {
  return @_ if $? == 0;
  say STDERR "Child process is failed with $? status"; exit $?
}

chomp(my @displays = do {
  my $re = qr/ (\d+)x(\d+)\+(\d+)\+(\d+) /;
  sort { %{$a}{x} <=> %{$b}{x} }
  map { /$re/; my %x = (w => $1, h => $2, x => $3, y => $4); \%x }
  grep { /$re/ } safe `xrandr --current`
});

GetOptions(

  "help|?" => \$show_help,

  "display=i" => sub {
    if ($_[1] < 1 || $_[1] > scalar(@displays)) {
      say STDERR "Unacceptable display number: '$_[1]'";
      fail_usage;
    }

    $display = $_[1];
  },

  "place|cursor-place=s" => sub {
    my $x = $_[1];

    if (! first { $_ eq $x } @cursor_places) {
      say STDERR "Unknown cursor place value: '$x'";
      fail_usage;
    }

    $cursor_place = $x;
  },

) || fail_usage;

unless (defined $display) {
  say STDERR "'display' option is required";
  fail_usage;
}

pod2usage(-exitval => 2, -verbose => 2) if $show_help;
my %params = %{$displays[$display - 1]};
my @pos = split '', $cursor_place;
my ($xp, $yp) = ($pos_map{$pos[0]}, $pos_map{$pos[1]});
my $x = $params{x} + ($params{w} * $xp / 100);
my $y = $params{y} + ($params{h} * $yp / 100);
system qw(xdotool mousemove), $x, $y;

__END__

=encoding UTF-8

=head1 DESCRIPTION

Move mouse cursor to another screen at specific place.

=head1 SYNOPSIS

cursor-to-display.pl [options]

  Options:
    --help -h
      Show this usage info

    --display=[number] -d=[number]
      Display ordering number to move mouse cursor at

    --place=[value] --cursor-place=[value] -p=[value]
      Move cursor at specific place

      Possible `value`s:
        "lt" for (l)eft-(t)op
        "ct" for (c)enter-(t)op
        "rt" for (r)ight-(t)op
        "lc" for (l)eft-(c)enter
        "cc" for (c)enter-(c)enter
        "rc" for (r)ight-(c)enter
        "lb" for (l)eft-(b)ottom
        "cb" for (c)enter-(b)ottom
        "rb" for (r)ight-(b)ottom

=cut
