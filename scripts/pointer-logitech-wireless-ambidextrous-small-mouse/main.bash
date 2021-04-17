# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

HAND=${HAND:-left}
XINPUT_DEVICE_PRODUCT_ID=${XINPUT_DEVICE_PRODUCT_ID:-'1133, 16469'}

if [[ $HAND != left ]] && [[ $HAND != right ]]; then
	>&2 printf 'Incorrect $HAND value: "%s"\n' "$HAND"
	exit 1
fi

xinput list --short \
	| grep -oP '↳ Logitech Wireless Mouse\s+id=\K\d+(?=\s)' \
	| while read id; do
		id=$[$id]
		device_id=$(
			xinput list-props "$id" | grep -oP 'Device Product ID \(\d+\):\s+\K.+\s*$'
		)
		if
			[[ $XINPUT_DEVICE_PRODUCT_ID == $device_id ]] && \
			{ xinput list-props "$id" \
				| grep -P 'libinput Left Handed Enabled \(\d+\):' >/dev/null; }
		then
			if [[ $HAND == left ]]; then x=1; else x=0; fi
			xinput set-prop "$id" 'libinput Left Handed Enabled' "$x"
		fi
	done
