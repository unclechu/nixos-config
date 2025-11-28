#! /usr/bin/env bash
set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail

# Open Alacritty to prompt some arguments for the `tmuxed-alacritty-new.sh`
# call first and then spawn a new Tmuxed Alacritty session with those arguments.
#
# Usage:
#   ./tmuxed-alacritty-new-prompt.sh

: "${ALACRITTY_EXE:=alacritty}"
: "${TMUXED_ALACRITTY_EXE:=./tmuxed-alacritty-new.sh}"

# Guard dependencies
(X=$ALACRITTY_EXE; if ! &>/dev/null type -P -- "$X" && ! [[ -r $X && -x $X ]]; then
	>&2 printf '[FAIL] Missing ALACRITTY_EXE “%s” dependency\n' "$X"
	exit 1
fi)
(X=$TMUXED_ALACRITTY_EXE; if ! &>/dev/null type -P -- "$X" && ! [[ -r $X && -x $X ]]; then
	>&2 printf '[FAIL] Missing TMUXED_ALACRITTY_EXE “%s” dependency\n' "$X"
	exit 1
fi)
>/dev/null type bash sed xprop xdotool realpath nohup dirname basename

BASH_EXE=$(type -P bash)
NOHUP_EXE=$(type -P nohup)

TMUXED_ALACRITTY_EXE_ABSOLUTE=$(
	if EXE_PATH=$(2>/dev/null type -P -- "$TMUXED_ALACRITTY_EXE"); then
		realpath -- "$EXE_PATH"
	else
		realpath -- "$TMUXED_ALACRITTY_EXE"
	fi
)

TMUXED_ALACRITTY_DIR=$(dirname -- "$TMUXED_ALACRITTY_EXE_ABSOLUTE")
TMUXED_ALACRITTY_FILE=$(basename -- "$TMUXED_ALACRITTY_EXE_ABSOLUTE")

# Improved command debug tracing
PS4='+ [${BASH_SOURCE##*/}:${LINENO}] '

# Getting working directory for the new terminal
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
	-e "$BASH_EXE" -c $"
		set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
		PROMPT=$'\e[33m'${TMUXED_ALACRITTY_DIR@Q}$'/\e[31m⮧\e[0m\n'
		PROMPT=\"\${PROMPT}\"$'\e[33m'${TMUXED_ALACRITTY_FILE@Q}$'\e[0m '
		read -er -i '_ ' -p \"\$PROMPT\" ARGS_CODE
		SHELL_CODE=${TMUXED_ALACRITTY_EXE_ABSOLUTE@Q}
		SHELL_CODE=\"\$SHELL_CODE WORKING_DIRECTORY=${WORKING_DIRECTORY@Q}\"
		SHELL_CODE=\"\$SHELL_CODE \$ARGS_CODE\"
		set -o xtrace
		<&- &>/dev/null exec ${NOHUP_EXE@Q} ${BASH_EXE@Q} \
			-c \"eval -- \${SHELL_CODE@Q} & disown\"
	"
)

set -o xtrace
exec "${CMD[@]}"
