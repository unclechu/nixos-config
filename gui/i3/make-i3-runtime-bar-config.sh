#! /usr/bin/env bash
# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail

# Guard dependencies
>/dev/null type jq
>/dev/null type i3-msg
>/dev/null type pidof
>/dev/null type wenzels-i3-status-generator

if ! pidof i3 >/dev/null; then
	# Can’t call `i3-msg -t get_outputs`
	>&2 printf 'i3 is not running, not doing anything'
	exit 0
fi

DISPLAY_NUM_FILE=$HOME/.pseudo-primary-display
PSEUDO_PRIMARY_DISPLAY_I3_CONFIG=$XDG_RUNTIME_DIR/i3-runtime-bar-config.conf

if [[ -f "$DISPLAY_NUM_FILE" ]]; then
	DISPLAY_NUM=$(<"$DISPLAY_NUM_FILE")
	TRAY_OUTPUT=$(
		i3-msg -t get_outputs \
			| jq -r --argjson display_num "$DISPLAY_NUM" \
				'(map(select(.active)) | sort_by(.rect.x, .rect.y) | map(.name))[$display_num-1]'
	)
else
	TRAY_OUTPUT=primary
fi

NEW_CONTENTS=$(
	printf '
		bar {
			status_command wenzels-i3-status-generator
			position top
			tray_output %s
		}
	' "$TRAY_OUTPUT"
)

was_file_updated=false

if [[ -f "$PSEUDO_PRIMARY_DISPLAY_I3_CONFIG" ]]; then
	CUR_CONTENTS=$(<"$PSEUDO_PRIMARY_DISPLAY_I3_CONFIG")
	if [[ "$NEW_CONTENTS" != "$CUR_CONTENTS" ]]; then
		printf '%s\n' "$NEW_CONTENTS" > "$PSEUDO_PRIMARY_DISPLAY_I3_CONFIG"
		was_file_updated=true
	fi
else
	printf '%s\n' "$NEW_CONTENTS" > "$PSEUDO_PRIMARY_DISPLAY_I3_CONFIG"
	was_file_updated=true
fi

if "$was_file_updated"; then
	(set -o xtrace; i3-msg reload)
fi
