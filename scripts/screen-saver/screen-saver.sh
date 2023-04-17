#! /usr/bin/env bash
# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# This script switches on and off the screen saver (blanking of the screen).
#
# See for more information about DPMS:
# - https://wiki.archlinux.org/title/Display_Power_Management_Signaling
# - https://unix.stackexchange.com/a/180552

set -o errexit || exit
set -o nounset
set -o pipefail

action=on
blank_screen_timeout=$(( 5 * 60 )) # 5 minutes in seconds

for arg in "$@"; do
	if [[ $arg =~ ^[0-9]+$ ]]; then
		blank_screen_timeout=$arg
	elif [[ $arg =~ ^(on|off)$ ]]; then
		action=$arg
	else
		>&2 printf 'Incorrect argument: "%s"\n' "$arg"
		exit 1
	fi
done

(
	set -o xtrace
	xset dpms 0 0 0
	xset s noblank
	xset s off
)

if [[ $action == on ]]; then
	(
		set -o xtrace
		xset +dpms
		xset s blank
		xset s "$blank_screen_timeout"
	)
fi
