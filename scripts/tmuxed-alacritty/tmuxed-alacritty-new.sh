#! /usr/bin/env bash
set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail

# Start new “Tmuxed Alacritty” session.
#
# Usage:
#   ./tmuxed-alacritty-new.sh [SESSION_NAME]
#   ./tmuxed-alacritty-new.sh SESSION_NAME CMD [CMD_ARG_1 [CMD_ARG_N…]]
#
# If SESSION_NAME is not specified it will be generated, incrementing the
# number (e.g. “s1”, “s2”, etc.)
#
# You can also set SESSION_NAME to “_” to let the script generate the session
# name for you when you need to specify a command to run. For example:
#   ./tmuxed-alacritty-new.sh _ mc
#
# Start a mpvc daemon and TUI and a tmuxed Alacritty window/session example:
#   ./tmuxed-alacritty-new.sh music mpvc-tui -T

: "${TMUX_EXE:=tmux}"
: "${ALACRITTY_EXE:=alacritty}"

# Guard dependencies
>/dev/null type sed sort tail "$TMUX_EXE" "$ALACRITTY_EXE"
>/dev/null type xdotool xprop

gen-session-name() {
	set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail

	local last_num; last_num=$(
		(tmux list-sessions -F '#{session_name}' 2>/dev/null || true) \
		| sed -n 's/^s\([0-9]\+\)$/\1/p' \
		| sort -n \
		| tail -n 1
	)

	if [[ -z $last_num ]]; then
		echo s1
	else
		printf 's%d\n' "$(( last_num + 1 ))"
	fi
}

if (( $# == 0 )); then
	# No session name provided, auto-generating one

	new_session_name=$(gen-session-name)

elif (( $# == 1 )) && [[ $1 == --help || $1 == -h ]]; then
	# Print usage info (extracted from the comment on top of this script)

	USAGE=$(<"$0" sed -n '
		1d

		:find
		/^[[:space:]]*#/b print
		n
		b find

		:print
		/^[[:space:]]*#/ {
			s/^[[:space:]]*#\(\s\|$\)//
			p
			n
			b print
		}
		q
	')
	printf '%s\n' "${USAGE//"./tmuxed-alacritty-new.sh"/"${0@Q}"}"
	exit 0

elif (( $# >= 1 )) && [[ -n $1 ]]; then
	# Session new provided

	if "$TMUX_EXE" has-session -t "$1" 2>/dev/null; then
		>&2 printf 'Error: tmux session “%s” already exists.\n' "$1"
		exit 1
	fi

	new_session_name=$1; shift
	if [[ $new_session_name == _ ]]; then
		new_session_name=$(gen-session-name)
	fi

else
	>&2 printf 'Unexpected argument: “%s”\n\n' "$@"
	>&2 "$0" --help
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

CMD=(
	"$ALACRITTY_EXE"
	--working-directory "$WORKING_DIRECTORY"
	-e "$TMUX_EXE" new-session
	-s "$new_session_name"
	-c "$WORKING_DIRECTORY"
	"$@"
)

exec "${CMD[@]}"
