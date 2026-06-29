#! /usr/bin/env bash
# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail

# Guard dependencies
>/dev/null type jq
>/dev/null type i3-msg
>/dev/null type pidof

if ! pidof i3 >/dev/null; then
	# Can’t call `i3-msg -t get_outputs`
	>&2 printf 'i3 is not running, not doing anything'
	exit 0
fi

DISPLAY_NUM_FILE=$HOME/.pseudo-primary-display
PSEUDO_PRIMARY_DISPLAY_I3_CONFIG=$XDG_RUNTIME_DIR/pseudo-primary-display-i3.conf

if [[ -f "$DISPLAY_NUM_FILE" ]]; then
	DISPLAY_NUM=$(<"$DISPLAY_NUM_FILE")
	DISPLAY_NAME=$(
		i3-msg -t get_outputs \
			| jq -r --argjson display_num "$DISPLAY_NUM" \
				'[.[] | select(.active) | .name][$display_num-1]'
	)
	# shellcheck disable=SC2016
	printf '
		bar {
			status_command wenzels-i3-status-generator
			position top
			tray_output %s
		}
	' "$DISPLAY_NAME" > "$PSEUDO_PRIMARY_DISPLAY_I3_CONFIG"
else
	echo > "$PSEUDO_PRIMARY_DISPLAY_I3_CONFIG"
fi
