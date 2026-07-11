#! /usr/bin/env bash
# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail

# Guard dependencies
>/dev/null type pacmd
>/dev/null type pactl
>/dev/null type grep
>/dev/null type sed
>/dev/null type awk
>/dev/null type xargs
>/dev/null type basename
>/dev/null type dzen-box

COMMANDS=(mute unmute mute-toggle inc dec reset)

show-usage() {
	echo
	echo Usage:
	printf '  %s COMMAND\n' "$(basename -- "$0")"
	echo
	echo Available COMMANDs:
	printf '  %s\n' "${COMMANDS[@]}"
	echo
}

if (( $# < 1 )); then
	>&2 echo Incorrect arguments! Provide a command!
	>&2 show-usage
	exit 1
fi

SINK=$(pactl info | grep -i 'default sink:' | sed 's/^default sink:[ ]*//i')

case $1 in
	mute)
		if (( $# != 1 )); then >&2 echo Incorrect arguments; exit 1; fi
		pactl set-sink-mute "$SINK" true
		;;
	unmute)
		if (( $# != 1 )); then >&2 echo Incorrect arguments; exit 1; fi
		pactl set-sink-mute "$SINK" false
		;;
	mute-toggle)
		if (( $# != 1 )); then >&2 echo Incorrect arguments; exit 1; fi
		pactl set-sink-mute "$SINK" toggle
		;;
	inc)
		if (( $# < 1 || $# > 2 )); then
			>&2 echo Incorrect arguments
			exit 1
		fi

		x=$(
			if (( $# == 2 ))
			then printf -- '+%s' "$2"
			else printf -- '+1.0dB'
			fi
		)

		pactl set-sink-mute   "$SINK" false
		pactl set-sink-volume "$SINK" "$x"
		;;
	dec)
		if (( $# < 1 || $# > 2 )); then
			>&2 echo Incorrect arguments
			exit 1
		fi

		x=$(
			if (( $# == 2 ))
			then printf -- '-%s' "$2"
			else printf -- '-1.0dB'
			fi
		)

		pactl set-sink-mute   "$SINK" false
		pactl set-sink-volume "$SINK" "$x"
		;;
	reset)
		if (( $# != 1 )); then >&2 echo Incorrect arguments; exit 1; fi

		# reset devices outputs volumes
		pactl list sinks short \
			| awk '{print $2}' \
			| xargs -I {} pactl set-sink-volume '{}' 0db

		# reset devices inputs volumes
		pactl list sources short \
			| awk '{print $2}' \
			| xargs -I {} pactl set-source-volume '{}' 0db

		# reset applications outputs volumes
		pactl list sink-inputs short \
			| awk '{print $1}' \
			| xargs -I {} pactl set-sink-input-volume '{}' 0db

		# reset applications inputs volumes
		pactl list source-outputs short \
			| awk '{print $1}' \
			| xargs -I {} pactl set-source-output-volume '{}' 0db
		;;
	-h|--help|help)
		show-usage
		exit
		;;
	*)
		>&2 printf 'Unknown command: "%s"!\n' "$1"
		>&2 show-usage
		exit 1
		;;
esac

# dzen-box indicator for current volume setting

sinks=$(pacmd list-sinks)

re_in_between='([
]	\s*[^
]+)+[
]	\s*'

re="^.*[
]  \* index: [0-9]+[
].*\s+name: <$SINK>${re_in_between}volume: ([^
]+)${re_in_between}muted: (yes|no)[
].*$"

if [[ $sinks =~ $re ]]; then
	re='^.* ([0-9]+)% .* ([0-9]+)% .*$'
	if [[ ${BASH_REMATCH[4]} == yes ]]; then
		dzen-box MUTE lightblue & disown
	elif [[ ${BASH_REMATCH[2]} =~ $re ]]; then
		dzen-box $(( (BASH_REMATCH[1] + BASH_REMATCH[2]) / 2 ))% lightblue & disown
	fi
fi
