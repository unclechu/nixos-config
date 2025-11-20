#! /usr/bin/env bash
set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail

# Select and attach to an existing “Tmuxed Alacritty” session.

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
>/dev/null type sort head cut grep "$TMUX_EXE" "$ALACRITTY_EXE" "$SKIM_EXE"

if [[ ! -v IS_INNER ]]; then
	exec "$ALACRITTY_EXE" -e "$0" INNER "$TMUX_EXE" "$ALACRITTY_EXE" "$SKIM_EXE"
fi

SESSION_LINE_TEMPLATE=$'#{session_name}\t#{session_attached}\t#{?session_attached,●,○}'
SESSION_LINE_TEMPLATE=$SESSION_LINE_TEMPLATE' #{session_name} (#{session_windows} windows)'

SESSIONS_RAW=$("$TMUX_EXE" list-sessions -F "$SESSION_LINE_TEMPLATE" 2>/dev/null || true)

if [[ -z $SESSIONS_RAW ]]; then
	>&2 echo 'Error: No existing tmux sessions found.'
	exit 1
fi

# Sort: unattached (0) first by column 2, then by name (column 1), tab-delimited
SESSIONS_SORTED=$(<<<"$SESSIONS_RAW" sort -t $'\t' -k2,2n -k1,1)

# Extract only the display part (column 3 onwards) for Skim
DISPLAY_LINES=$(<<<"$SESSIONS_SORTED" cut -f3-)

PROMPT='attach to a tmux session'
CHOICE=$(<<<"$DISPLAY_LINES" "$SKIM_EXE" --prompt="$PROMPT > " --no-multi) || exit 0

# Map the chosen display line back to the full record
SELECTED_RECORD=$(<<<"$SESSIONS_SORTED" grep -F -- "$CHOICE" | head -n 1)

# Extract the **session name** from column 1 (tab-delimited)
SELECTED_SESSION=$(<<<"$SELECTED_RECORD" cut -f1)

# Selection was cancelled by the user (e.g. Escape button)
if [[ -z $SELECTED_SESSION ]]; then
	exit 0
fi

exec "$TMUX_EXE" attach-session -t "$SELECTED_SESSION"
