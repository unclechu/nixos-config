#! /usr/bin/env bash
# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
set -o errexit || exit; set -o nounset; set -o pipefail
SCRIPT_DIR=$(dirname -- "${BASH_SOURCE[0]}"); cd -- "$SCRIPT_DIR"

# My home audio mains+subwoofers x-over setup.
#
# Usage example:
#   ./home-audio-lh-xover.sh
#
# Only reconfigure “mains” for already running setup:
#   ./home-audio-lh-xover.sh mains fr
#   ./home-audio-lh-xover.sh mains band

# Guard dependencies
>/dev/null type jalv.gtk3
>/dev/null type calfjackhost
>/dev/null type jack_lsp
>/dev/null type jack_disconnect
>/dev/null type jack_connect
>/dev/null type sleep

# Environment variable parameters

# Or 'PulseAudio JACK Sink-01'
: "${PA_SINK_CLIENT:=PulseAudio JACK Sink}"

: "${JALV_LSP_XOVER_PRESET:=presets/lsp-lh-xover-jalv}"
: "${JALV_LSP_XOVER_CLIENT:=home-audio-lsp-lh-xover-jalv}"

: "${CALFJACKHOST_PRESET:=presets/calfjackhost.xml}"
: "${CALFJACKHOST_CLIENT:=home-audio-calf}"

# `fr` for full range signal to main speakers.
# `band` for sending only signal above sub cut-off frequency.
: "${MAINS_CONFIGURATION:=band}"

# Command-line arguments parsing

if (( $# == 0 )); then
	MODE=default
elif (( $# == 2 )) && [[ $1 == mains ]]; then
	MODE=$1
	MAINS_CONFIGURATION=$2
else
	>&2 printf 'Unexpected argument: “%s”\n' "$@"
	exit 1
fi

# Setting it up

if [[ $MODE == default ]]; then
	(
		CMD=(
			jalv.gtk3
			--jack-name="$JALV_LSP_XOVER_CLIENT"
			http://lsp-plug.in/plugins/lv2/crossover_stereo
			--load="$JALV_LSP_XOVER_PRESET"
			# --no-menu
			--generic-ui # Save CPU from heavy UI rendering laod
		)
		set -o xtrace
		"${CMD[@]}" & disown
	)

	(
		CMD=(
			calfjackhost
			-c "$CALFJACKHOST_CLIENT"
			-l "$CALFJACKHOST_PRESET"
		)
		set -o xtrace
		"${CMD[@]}" & disown
	)
fi

PORTS=(
	"$JALV_LSP_XOVER_CLIENT:in_l"
	"$JALV_LSP_XOVER_CLIENT:in_r"
	"$JALV_LSP_XOVER_CLIENT:band0l"
	"$JALV_LSP_XOVER_CLIENT:band0r"
	"$JALV_LSP_XOVER_CLIENT:band1l"
	"$JALV_LSP_XOVER_CLIENT:band1r"
	# Don’t really care about these
	# "$JALV_LSP_XOVER_CLIENT:band2l"
	# "$JALV_LSP_XOVER_CLIENT:band2r"
	"$CALFJACKHOST_CLIENT:eq In #1"
	"$CALFJACKHOST_CLIENT:eq In #2"
	"$CALFJACKHOST_CLIENT:eq Out #1"
	"$CALFJACKHOST_CLIENT:eq Out #2"
	"$CALFJACKHOST_CLIENT:sub-stereo In #1"
	"$CALFJACKHOST_CLIENT:sub-stereo In #2"
	"$CALFJACKHOST_CLIENT:sub-stereo Out #1"
	"$CALFJACKHOST_CLIENT:sub-stereo Out #2"
	"$CALFJACKHOST_CLIENT:mains-stereo In #1"
	"$CALFJACKHOST_CLIENT:mains-stereo In #2"
	"$CALFJACKHOST_CLIENT:mains-stereo Out #1"
	"$CALFJACKHOST_CLIENT:mains-stereo Out #2"
)

if [[ $MODE != mains ]]; then
	while true; do
		echo 'Waiting for ports…'
		ALL_READY=1
		declare -A ready_ports=()
		for port in "${PORTS[@]}"; do
			present_ports=$(jack_lsp)
			readarray -t present_ports_arr <<<"$present_ports"
			found=0
			for present_port in "${present_ports_arr[@]}"; do
				if [[ $present_port == "$port" ]]; then
					found=1
					if [[ ${ready_ports["$port"]-0} != 1 ]]; then
						printf 'Port “%s” is ready\n' "$port"
					fi
					ready_ports["$port"]=1
					break
				fi
			done
			if (( found != 1 )); then
				ALL_READY=0
				break
			fi
		done
		if (( ALL_READY == 1 )); then
			echo 'All ports are ready!'
			break
		else
			(set -o xtrace; sleep .5s)
		fi
	done
fi

set-mains-configuration() (
	if ! (( $# == 1 )); then (set -o xtrace; (( $# == 1 ))) fi

	set -o xtrace

	# Full range into main speakers
	if [[ $1 == fr ]]; then
		jack_disconnect "$JALV_LSP_XOVER_CLIENT:band1l" "$CALFJACKHOST_CLIENT:mains-stereo In #1" || true
		jack_disconnect "$JALV_LSP_XOVER_CLIENT:band1r" "$CALFJACKHOST_CLIENT:mains-stereo In #2" || true
		jack_connect "$JALV_LSP_XOVER_CLIENT:out_l" "$CALFJACKHOST_CLIENT:mains-stereo In #1"
		jack_connect "$JALV_LSP_XOVER_CLIENT:out_r" "$CALFJACKHOST_CLIENT:mains-stereo In #2"

	# Bass cut into main speakers
	elif [[ $1 == band ]]; then
		jack_disconnect "$JALV_LSP_XOVER_CLIENT:out_l" "$CALFJACKHOST_CLIENT:mains-stereo In #1" || true
		jack_disconnect "$JALV_LSP_XOVER_CLIENT:out_r" "$CALFJACKHOST_CLIENT:mains-stereo In #2" || true
		jack_connect "$JALV_LSP_XOVER_CLIENT:band1l" "$CALFJACKHOST_CLIENT:mains-stereo In #1"
		jack_connect "$JALV_LSP_XOVER_CLIENT:band1r" "$CALFJACKHOST_CLIENT:mains-stereo In #2"

	else
		>&2 printf 'Unexpected mains configuration: “%s”\n' "$1"
		return 1
	fi
)

# Only reconfigure mains
if [[ $MODE == mains ]]; then
	set-mains-configuration "$MAINS_CONFIGURATION"
	exit 0
fi

set -o xtrace

sleep 1s

jack_disconnect "$PA_SINK_CLIENT:front-left" 'system:playback_1' || true
jack_disconnect "$PA_SINK_CLIENT:front-right" 'system:playback_2' || true
jack_disconnect "$PA_SINK_CLIENT:front-left" 'system:playback_3' || true
jack_disconnect "$PA_SINK_CLIENT:front-right" 'system:playback_4' || true
jack_disconnect "$PA_SINK_CLIENT:front-left" 'system:playback_5' || true
jack_disconnect "$PA_SINK_CLIENT:front-right" 'system:playback_6' || true
jack_disconnect "$PA_SINK_CLIENT:front-left" 'system:playback_7' || true
jack_disconnect "$PA_SINK_CLIENT:front-right" 'system:playback_8' || true

# Pre EQ inputs
jack_connect "$PA_SINK_CLIENT:front-left" "$CALFJACKHOST_CLIENT:eq In #1"
jack_connect "$PA_SINK_CLIENT:front-right" "$CALFJACKHOST_CLIENT:eq In #2"

# Inputs
jack_connect "$CALFJACKHOST_CLIENT:eq Out #1" "$JALV_LSP_XOVER_CLIENT:in_l"
jack_connect "$CALFJACKHOST_CLIENT:eq Out #2" "$JALV_LSP_XOVER_CLIENT:in_r"

# Sub-woofer
jack_connect "$JALV_LSP_XOVER_CLIENT:band0l" "$CALFJACKHOST_CLIENT:sub-stereo In #1"
jack_connect "$JALV_LSP_XOVER_CLIENT:band0r" "$CALFJACKHOST_CLIENT:sub-stereo In #2"
jack_connect "$CALFJACKHOST_CLIENT:sub-stereo Out #1" 'system:playback_3'
# jack_connect "$CALFJACKHOST_CLIENT:sub-stereo Out #2" 'system:playback_3' # mono
jack_connect "$CALFJACKHOST_CLIENT:sub-stereo Out #2" 'system:playback_4' # stereo

# Highs (mids + highs)
set-mains-configuration "$MAINS_CONFIGURATION"
jack_connect "$CALFJACKHOST_CLIENT:mains-stereo Out #1" 'system:playback_5'
jack_connect "$CALFJACKHOST_CLIENT:mains-stereo Out #2" 'system:playback_6'
