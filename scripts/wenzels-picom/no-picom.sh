#! /usr/bin/env bash
set -o errexit || exit; set -o nounset; set -o pipefail
exec <&- # Close standard input

# Guard dependencies
>/dev/null type sleep
>/dev/null type pkill
>/dev/null type sh # For Feh run
[[ -v USER ]] || (set -o xtrace; [[ -v USER ]])

# Terminate possibly running Picom
(set -o xtrace; pkill -x -U "$USER" -- picom 2>/dev/null) || true

# Give it a second to start
(set -o xtrace; sleep 1s)

# Update Feh-managed wallpaper
if [[ -f ~/.fehbg ]]; then
	if [[ -x ~/.fehbg ]]; then
		(set -o xtrace; ~/.fehbg & disown)
	else
		(set -o xtrace; sh ~/.fehbg & disown)
	fi
fi
