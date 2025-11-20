#! /usr/bin/env bash
set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail

# Start new “Tmuxed Alacritty” session.

: "${TMUX_EXE:=tmux}"
: "${ALACRITTY_EXE:=alacritty}"

# Guard dependencies
>/dev/null type sed sort tail "$TMUX_EXE" "$ALACRITTY_EXE"
>/dev/null type xdotool xprop

if (( $# == 0 )); then
	# No session name provided, auto-generating one

	last_num=$(
		(tmux list-sessions -F '#{session_name}' 2>/dev/null || true) \
		| sed -n 's/^s\([0-9]\+\)$/\1/p' \
		| sort -n \
		| tail -n 1
	)

	if [[ -z $last_num ]]; then
		new_session_name=s1
	else
		new_session_name="s$(( last_num + 1 ))"
	fi

elif (( $# == 1 )) && [[ -n $1 ]]; then
	# Session new provided

	if "$TMUX_EXE" has-session -t "$1" 2>/dev/null; then
		>&2 printf 'Error: tmux session “%s” already exists.\n' "$1"
		exit 1
	fi

	new_session_name=$1

else
	>&2 printf 'Unexpected argument: “%s”\n' "$@"
	>&2 printf 'Usage: %s (NEW_SESSION_NAME)\n' "$0"
	exit 1
fi

if ACTIVE_WINDOW_ID=$(xdotool getactivewindow); then
	TMUX_PANE_CWD=$(
		xprop -id "$ACTIVE_WINDOW_ID" TMUX_PANE_CWD 2>/dev/null \
		| sed -n 's/^TMUX_PANE_CWD(STRING) = "\(.\+\)"$/\1/p'
	)

	if [[ -n $TMUX_PANE_CWD ]]; then
		WORKING_DIRECTORY=$TMUX_PANE_CWD
	else
		# Last focused window might be not a terminal emulator
		# and would not have this property set then.
		WORKING_DIRECTORY=$HOME
	fi
else
	# For an empty workspace there will be a failure to `getactivewindow`.
	# It is normal, just defaulting to HOME.
	WORKING_DIRECTORY=$HOME
fi

exec "$ALACRITTY_EXE" --working-directory "$WORKING_DIRECTORY" -e \
	"$TMUX_EXE" new-session -s "$new_session_name" -c "$WORKING_DIRECTORY"
