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
>/dev/null type cp
>/dev/null type systemctl
>/dev/null type add-i3-pseudo-primary-display-runtime-config

DISPLAY_NUM_FILE=$HOME/.pseudo-primary-display

# Some applications need to be very responsive.
# Reading the file from the disk is theoretically more expensive/slower
# than from `$XDG_RUNTIME_DIR` which is typically mounted as tmpfs (in RAM).
#
# WARNING! Make sure your X11 session auto-start script
# copies `$DISPLAY_NUM_FILE` to `$DISPLAY_NUM_RUNTIME_FILE`!
# See for `copyToRuntimeScript` helper in `default.nix`.
DISPLAY_NUM_RUNTIME_FILE=$XDG_RUNTIME_DIR/pseudo-primary-display

DISPLAYS_COUNT=$(
	xrandr --listmonitors | awk '$2 ~ /^\+/ { count++ } END { print count }'
)

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
	(
		set -o xtrace
		rm -vf -- "$DISPLAY_NUM_FILE" "$DISPLAY_NUM_RUNTIME_FILE"
	)
else
	>&2 printf 'Setting display %d as pseudo-primary…\n' "$DISPLAY_NUM"
	printf %d "$DISPLAY_NUM" > "$DISPLAY_NUM_FILE"

	# WARNING! Make sure your X11 session autostart script does the same!
	# See for `copyToRuntimeScript` helper in `default.nix`.
	cp -vf -- "$DISPLAY_NUM_FILE" "$DISPLAY_NUM_RUNTIME_FILE"
fi

>&2 echo 'Restarting Dunst daemon…'
(set -o xtrace; systemctl restart --user dunst)

if XMONAD_PID=$(pidof xmonad); then
	>&2 echo 'Shallowly restaring XMonad…'
	(set -o xtrace; "/proc/$XMONAD_PID/exe" ctl restart-xmonad shallow)
fi

if pidof i3 >/dev/null; then
	>&2 echo 'Restaring i3…'
	(
		set -o xtrace
		add-i3-pseudo-primary-display-runtime-config
		i3-msg reload
	)
fi
