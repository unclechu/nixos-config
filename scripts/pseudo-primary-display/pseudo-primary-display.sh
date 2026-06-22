#! /usr/bin/env bash
# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail

# Set a specified display as pseudo-primary script.
#
# Changing actual X11 primary display is messing up the displays ordering
# which doesn’t play nicely with my place-cursor-at, XMonad switch-to-display
# hotkeys, etc.
#
# The pseudo-primary display determines where Dunst is showing its notifications
# and where Polybar is showing the system tray. My home setup features multiple
# displays placed on different tables. Sometimes I go to another table and want
# my notifications and system tray to see there. So I want to be able to
# dynamically change the pseudo-primary. This scripts provides this.
# It makes the necessary changes and restarts Dunst and XMonad (which results
# into restarting Polybar).
#
# Usage examples:
#
#   # Reset pseudo-primary display and use actual real primary one.
#   ./pseudo-primary-display.sh
#
#   # Set display 2 as pseudo-primary.
#   ./pseudo-primary-display.sh 2

# Guard dependencies
>/dev/null type xrandr
>/dev/null type dunstctl
>/dev/null type pidof
>/dev/null type awk
>/dev/null type rm
>/dev/null type systemctl

DISPLAY_NUM_FILE=$HOME/.pseudo-primary-display

DISPLAYS_COUNT=$(
	xrandr --listmonitors | awk '$2 ~ /^\+/ { count++ } END { print count }'
)

XMONAD_PID=$(pidof xmonad)
XMONAD_CTL=("/proc/$XMONAD_PID/exe" ctl)
DUNST_CONF=$XDG_RUNTIME_DIR/dunst-pseudo-primary-display.conf

if ! [[ $DISPLAYS_COUNT =~ ^[1-9]$ ]]; then
	>&2 printf 'Incorrect displays count number: “%s”' "$DISPLAYS_COUNT"
	exit 1
fi

if (( $# == 0 )); then
	DISPLAY_NUM=0
elif (( $# == 1 )) && [[ $1 =~ ^[1-9]$ ]]; then
	if (( $1 > DISPLAYS_COUNT )); then
		>&2 printf \
			'[ERR] Display number %d exceeds displays count: %d\n' \
			"$1" "$DISPLAYS_COUNT"
		exit 1
	fi
	DISPLAY_NUM=$1
else
	>&2 printf '[ERR] Incorrect argument: “%s”\n' "$@"
	exit 1
fi

if (( DISPLAY_NUM == 0 )); then
	>&2 echo 'Resetting the pseudo-primary display…'
	(set -o xtrace; rm -vf -- "$DISPLAY_NUM_FILE" "$DUNST_CONF")
else
	>&2 printf 'Setting display %d as pseudo-primary…\n' "$DISPLAY_NUM"
	printf %d "$DISPLAY_NUM" > "$DISPLAY_NUM_FILE"

	printf '
	[global]
	monitor = %d
	follow = none
	' "$(( DISPLAY_NUM - 1 ))" > "$DUNST_CONF"
fi

>&2 echo 'Restarting Dunst daemon…'
(set -o xtrace; systemctl restart --user dunst)

>&2 echo 'Shallowly restaring XMonad…'
(set -o xtrace; "${XMONAD_CTL[@]}" restart-xmonad shallow)
