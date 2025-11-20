#! /usr/bin/env bash
# shellcheck disable=SC1111
set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail

# Select and nuke existing tmux sessions.

if (( $# == 4 )) && [[ $1 == INNER && -n $2 && -n $3 && -n $4 ]]; then
	IS_INNER=1; shift
	TMUX_EXE=$1; shift
	ALACRITTY_EXE=$1; shift
	SKIM_EXE=$1; shift
elif (( $# == 0 )); then
	: "${TMUX_EXE:=tmux}"
	: "${ALACRITTY_EXE:=alacritty}"
	: "${SKIM_EXE:=sk}"
else
	>&2 printf 'Unexpected argument: %s\n' "$@"
	exit
fi

# Guard dependencies
>/dev/null type sort cut grep head "$TMUX_EXE" "$ALACRITTY_EXE" "$SKIM_EXE"

if [[ ! -v IS_INNER ]]; then
	exec "$ALACRITTY_EXE" -e "$0" INNER "$TMUX_EXE" "$ALACRITTY_EXE" "$SKIM_EXE"
fi

SESSION_LINE_TEMPLATE=$'#{session_name}\t#{session_attached}\t#{?session_attached,●,○}'
SESSION_LINE_TEMPLATE=$SESSION_LINE_TEMPLATE' #{session_name} (#{session_windows} windows)'

while :; do
	SESSIONS_RAW=$("$TMUX_EXE" list-sessions -F "$SESSION_LINE_TEMPLATE" 2>/dev/null || true)

	if [[ -z $SESSIONS_RAW ]]; then
		>&2 echo 'No tmux sessions found to nuke.'
		exit 1
	fi

	# Sort unattached first (col2), then by session name (col1)
	SESSIONS_SORTED=$(<<<"$SESSIONS_RAW" sort -t $'\t' -k2,2n -k1,1)

	# Only show the pretty part (col3+) to Skim
	DISPLAY_LINES=$(<<<"$SESSIONS_SORTED" cut -f3-)

	CHOICE=$(
		<<<"$DISPLAY_LINES" "$SKIM_EXE" --prompt='☠ NUKE ☠ tmux session > ' --no-multi
	) || exit 0  # ESC / Ctrl-C

	# Map back from chosen display line to the full record
	SELECTED_RECORD=$(<<<"$SESSIONS_SORTED" grep -F -- "$CHOICE" | head -n 1)

	SELECTED_SESSION=$(<<<"$SELECTED_RECORD" cut -f1)

	if [[ -z $SELECTED_SESSION ]]; then
		# User cancelled or something weird happened. Just exit.
		exit 0
	fi

	read -r -p "Kill tmux session “${SELECTED_SESSION}”? [y/N] " answer

	case "$answer" in
		[yY]|[yY][eE][sS])
			if ! "$TMUX_EXE" kill-session -t "$SELECTED_SESSION"; then
				>&2 printf 'Failed to kill tmux session “%s”.\n' "$SELECTED_SESSION"
				exit 1
			fi
			;;
		*)
			# Don’t nuke anything if not confirmed. Just re-open the list.
			:
			;;
	esac

	# Loop again: list is regenerated so nuked session disappears.
done
