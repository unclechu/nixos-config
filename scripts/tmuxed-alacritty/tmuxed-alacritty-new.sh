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
(X=$TMUX_EXE; if ! >/dev/null type -P -- "$X" && ! [[ -r $X && -x $X ]]; then
	>&2 printf '[FAIL] Missing TMUX_EXE “%s” dependency' "$X"
	exit 1
fi)
(X=$ALACRITTY_EXE; if ! >/dev/null type -P -- "$X" && ! [[ -r $X && -x $X ]]; then
	>&2 printf '[FAIL] Missing ALACRITTY_EXE “%s” dependency' "$X"
	exit 1
fi)
>/dev/null type sed sort tail sleep
>/dev/null type xdotool xprop

# Improved command debug tracing
PS4='+ [${BASH_SOURCE##*/}:${LINENO}] '

gen-session-name() {
	set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail

	local last_num; last_num=$(
		("$TMUX_EXE" list-sessions -F '#{session_name}' 2>/dev/null || true) \
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

# Support overriding WORKING_DIRECTORY via arguments
if (( $# > 0 )) && [[ $1 =~ ^WORKING_DIRECTORY=(.+)$ ]]; then
	working_directory=${BASH_REMATCH[1]}
	shift
fi

if (( $# == 0 )); then
	# No session name provided, auto-generating one

	new_session_name=_

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

	new_session_name=$1; shift

else
	>&2 printf 'Unexpected argument: “%s”\n\n' "$@"
	>&2 "$0" --help
	exit 1
fi

# Getting working directory for the new terminal
if [[ -v working_directory && -n $working_directory ]]; then
	: # Already set via arguments

elif ACTIVE_WINDOW_ID=$(xdotool getactivewindow); then
	TMUX_PANE_CWD=$(
		xprop -id "$ACTIVE_WINDOW_ID" TMUX_PANE_CWD 2>/dev/null \
		| sed -n 's/^TMUX_PANE_CWD(STRING) = "\(.\+\)"$/\1/p'
	)

	if [[ -n $TMUX_PANE_CWD ]]; then
		working_directory=$TMUX_PANE_CWD
	else
		# Last focused window might be not a terminal emulator
		# and would not have this property set then.
		working_directory=$HOME
	fi
else
	# For an empty workspace there will be a failure to `getactivewindow`.
	# It is normal, just defaulting to HOME.
	working_directory=$HOME
fi

spawn-tmux-session() {
	set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
	(( $# >= 1 )) && [[ -n $1 ]]
	local SESSION_NAME=$1; shift

	local NEW_SESSION_CMD; NEW_SESSION_CMD=(
		"$TMUX_EXE"
		new-session -d
		-s "$SESSION_NAME"
		-c "$working_directory"
		"$@"
	)

	(
		set -o xtrace
		"${NEW_SESSION_CMD[@]}" 2>&1
	)
}

attach-to-tmux-session() {
	set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
	(( $# == 1 )) && [[ -n $1 ]]
	local SESSION_NAME=$1; shift

	CMD=(
		"$ALACRITTY_EXE"
		--working-directory "$working_directory"
		-e "$TMUX_EXE" attach-session
		-t "$SESSION_NAME"
	)

	set -o xtrace
	exec "${CMD[@]}"
}


if [[ $new_session_name == _ ]]; then
	# Just giving a sane limit for retries
	RETRIES=50

	for (( attempt = 1; attempt <= RETRIES; ++attempt )); do
		new_session_name=$(gen-session-name)

		if error=$(spawn-tmux-session "$new_session_name" "$@"); then
			# `exec` inside will replace the script.
			attach-to-tmux-session "$new_session_name"

		elif [[ $error == "duplicate session: $new_session_name" ]]; then
			>&2 printf \
				'Attempt #%d to create “%s” tmux session failed, session name is already taken, retrying…\n' \
				"$attempt" \
				"$new_session_name"
			sleep .1s

		else
			>&2 printf \
				'[FAIL] Attempt #%d to create “%s” tmux session failed, unexpected error: %s\n' \
				"$attempt" \
				"$new_session_name" \
				"$error"
			exit 1
		fi
	done

	>&2 echo '[FAIL] Attempts limit to create a tmux session with a generated name are reached!'
	exit 1

else
	if error=$(spawn-tmux-session "$new_session_name" "$@"); then
		# `exec` inside will replace the script.
		attach-to-tmux-session "$new_session_name"

	elif [[ $error == "duplicate session: $new_session_name" ]]; then
		>&2 printf \
			'Session name “%s” is already taken, just attaching…\n' \
			"$new_session_name"

		# `exec` inside will replace the script.
		attach-to-tmux-session "$new_session_name"

	else
		>&2 printf \
			'[FAIL] Failed to create “%s” tmux session, unexpected error: %s\n' \
			"$new_session_name" \
			"$error"
		exit 1
	fi
fi
