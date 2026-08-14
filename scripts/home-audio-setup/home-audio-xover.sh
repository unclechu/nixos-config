#! /usr/bin/env bash
# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
set -o errexit || exit; set -o nounset; set -o pipefail
SCRIPT_DIR=$(dirname -- "${BASH_SOURCE[0]}"); cd -- "$SCRIPT_DIR"

# My home audio mains+subwoofers x-over setup.
#
# Usage examples:
#   ./home-audio-xover.sh lh
#   ./home-audio-xover.sh lmh
#   ./home-audio-xover.sh lmmh
#
# Only reconfigure “mains” for already running setup:
#   ./home-audio-xover.sh lh mains fr
#   ./home-audio-xover.sh lh mains band

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

: "${JALV_LSP_XOVER_PRESET:=presets/lsp-xover-jalv}"
: "${JALV_LSP_XOVER_CLIENT:=home-audio-lsp-xover-jalv}"

: "${CALFJACKHOST_PRESET:=presets/calfjackhost.xml}"
: "${CALFJACKHOST_CLIENT:=home-audio-calf}"

# `fr` for full range signal to main speakers.
# `band` for sending only signal above sub cut-off frequency.
: "${MAINS_CONFIGURATION:=band}"
# `stereo` for stereo pair of subs.
# `mono` for single sub with stereo sub channels summed into one.
: "${SUB_CONFIGURATION:=stereo}"

: "${SUB_STEREO_NAME:=sub-stereo}"
: "${MID_STEREO_NAME:=mid-stereo}"
: "${HIMID_STEREO_NAME:=hi-mid-stereo}"
: "${HI_STEREO_NAME:=hi-stereo}"

: "${HARDWARE_OUT_SUB_L:=system:playback_13}"
if [[ $SUB_CONFIGURATION == stereo ]]; then
	: "${HARDWARE_OUT_SUB_R:=system:playback_14}"
elif [[ $SUB_CONFIGURATION == mono ]]; then
	: "${HARDWARE_OUT_SUB_R:=$HARDWARE_OUT_SUB_L}"
else
	>&2 printf 'Unexpected SUB_CONFIGURATION value: “%s”\n' "$SUB_CONFIGURATION"
	exit 1
fi
: "${HARDWARE_OUT_MID_L:=system:playback_15}"
: "${HARDWARE_OUT_MID_R:=system:playback_16}"
: "${HARDWARE_OUT_HIMID_L:=system:playback_17}"
: "${HARDWARE_OUT_HIMID_R:=system:playback_18}"
: "${HARDWARE_OUT_HI_L:=system:playback_19}"
: "${HARDWARE_OUT_HI_R:=system:playback_20}"

SETUP_TARGET_VALUE_USAGE='(must be “lh”, “lmh”, or “lmmh”)'

if (( $# < 1 )); then
	>&2 printf 'Missing setup target argument %s\n' "$SETUP_TARGET_VALUE_USAGE"
	exit 1
elif [[ $1 != "lh" && $1 != "lmh" && $1 != "lmmh" ]]; then
	>&2 printf 'Unexpected setup target value %s: “%s”\n' "$SETUP_TARGET_VALUE_USAGE" "$1"
	exit 1
else
	SETUP_TARGET=$1
	shift
fi

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

if [[ $SETUP_TARGET == lh ]]; then
	EXTRA_BAND_PORTS=()
elif [[ $SETUP_TARGET == lmh || $SETUP_TARGET == lmmh ]]; then
	if [[ $SETUP_TARGET == lmmh ]]; then
		HIMID_PORTS=(
			"$JALV_LSP_XOVER_CLIENT:band3l"
			"$JALV_LSP_XOVER_CLIENT:band3r"
			"$CALFJACKHOST_CLIENT:$HIMID_STEREO_NAME In #1"
			"$CALFJACKHOST_CLIENT:$HIMID_STEREO_NAME In #2"
			"$CALFJACKHOST_CLIENT:$HIMID_STEREO_NAME Out #1"
			"$CALFJACKHOST_CLIENT:$HIMID_STEREO_NAME Out #2"
		)
	else
		HIMID_PORTS=()
	fi
	EXTRA_BAND_PORTS=(
		"$JALV_LSP_XOVER_CLIENT:band2l"
		"$JALV_LSP_XOVER_CLIENT:band2r"
		"$CALFJACKHOST_CLIENT:$HI_STEREO_NAME In #1"
		"$CALFJACKHOST_CLIENT:$HI_STEREO_NAME In #2"
		"$CALFJACKHOST_CLIENT:$HI_STEREO_NAME Out #1"
		"$CALFJACKHOST_CLIENT:$HI_STEREO_NAME Out #2"
		"${HIMID_PORTS[@]}"
	)
else
	>&2 printf 'Unexpected SETUP_TARGET value: “%s”\n' "$SETUP_TARGET"
	exit 1
fi

PORTS=(
	"$JALV_LSP_XOVER_CLIENT:in_l"
	"$JALV_LSP_XOVER_CLIENT:in_r"
	"$JALV_LSP_XOVER_CLIENT:band0l"
	"$JALV_LSP_XOVER_CLIENT:band0r"
	"$JALV_LSP_XOVER_CLIENT:band1l"
	"$JALV_LSP_XOVER_CLIENT:band1r"
	"$CALFJACKHOST_CLIENT:eq In #1"
	"$CALFJACKHOST_CLIENT:eq In #2"
	"$CALFJACKHOST_CLIENT:eq Out #1"
	"$CALFJACKHOST_CLIENT:eq Out #2"
	"$CALFJACKHOST_CLIENT:$SUB_STEREO_NAME In #1"
	"$CALFJACKHOST_CLIENT:$SUB_STEREO_NAME In #2"
	"$CALFJACKHOST_CLIENT:$SUB_STEREO_NAME Out #1"
	"$CALFJACKHOST_CLIENT:$SUB_STEREO_NAME Out #2"
	"$CALFJACKHOST_CLIENT:$MID_STEREO_NAME In #1"
	"$CALFJACKHOST_CLIENT:$MID_STEREO_NAME In #2"
	"$CALFJACKHOST_CLIENT:$MID_STEREO_NAME Out #1"
	"$CALFJACKHOST_CLIENT:$MID_STEREO_NAME Out #2"
	"${EXTRA_BAND_PORTS[@]}"
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
	set +o xtrace
	if ! (( $# == 1 )); then (set -o xtrace; (( $# == 1 ))) fi

	pids=()

	# Full range into main speakers
	if [[ $1 == fr ]]; then
		if [[ $SETUP_TARGET != lh ]]; then
			>&2 printf 'Full range setup does not make sense for this setup target: “%s”\n' "$SETUP_TARGET"
			exit 1
		fi

		(set -o xtrace; jack_disconnect "$JALV_LSP_XOVER_CLIENT:band1l" "$CALFJACKHOST_CLIENT:$MID_STEREO_NAME In #1" || :) & pids+=("$!")
		(set -o xtrace; jack_disconnect "$JALV_LSP_XOVER_CLIENT:band1r" "$CALFJACKHOST_CLIENT:$MID_STEREO_NAME In #2" || :) & pids+=("$!")
		(set -o xtrace; jack_connect "$JALV_LSP_XOVER_CLIENT:out_l" "$CALFJACKHOST_CLIENT:$MID_STEREO_NAME In #1") & pids+=("$!")
		(set -o xtrace; jack_connect "$JALV_LSP_XOVER_CLIENT:out_r" "$CALFJACKHOST_CLIENT:$MID_STEREO_NAME In #2") & pids+=("$!")

	# Bass cut into main speakers
	elif [[ $1 == band ]]; then
		(set -o xtrace; jack_disconnect "$JALV_LSP_XOVER_CLIENT:out_l" "$CALFJACKHOST_CLIENT:$MID_STEREO_NAME In #1" || :) & pids+=("$!")
		(set -o xtrace; jack_disconnect "$JALV_LSP_XOVER_CLIENT:out_r" "$CALFJACKHOST_CLIENT:$MID_STEREO_NAME In #2" || :) & pids+=("$!")
		(set -o xtrace; jack_connect "$JALV_LSP_XOVER_CLIENT:band1l" "$CALFJACKHOST_CLIENT:$MID_STEREO_NAME In #1") & pids+=("$!")
		(set -o xtrace; jack_connect "$JALV_LSP_XOVER_CLIENT:band1r" "$CALFJACKHOST_CLIENT:$MID_STEREO_NAME In #2") & pids+=("$!")

	else
		>&2 printf 'Unexpected mains configuration: “%s”\n' "$1"
		return 1
	fi

	>&2 printf 'Waiting for set-mains-configuration JACK port (dis)connection PIDs: %s\n' "${pids[*]}"
	wait -- "${pids[@]}"
)

# Only reconfigure mains
if [[ $MODE == mains ]]; then
	set-mains-configuration "$MAINS_CONFIGURATION"
	exit 0
fi

hardware_playback_ports=()
for port in "${present_ports_arr[@]}"; do
	if [[ $port =~ ^system:playback_[0-9]+$ ]]; then
		hardware_playback_ports+=("$port")
	fi
done

(set -o xtrace; sleep 1s)

pids=()

for playback_port in "${hardware_playback_ports[@]}"; do
	(set -o xtrace; jack_disconnect "$PA_SINK_CLIENT:front-left" "$playback_port" || :) & pids+=("$!")
	(set -o xtrace; jack_disconnect "$PA_SINK_CLIENT:front-right" "$playback_port" || :) & pids+=("$!")
done

# Pre EQ inputs
(set -o xtrace; jack_connect "$PA_SINK_CLIENT:front-left" "$CALFJACKHOST_CLIENT:eq In #1") & pids+=("$!")
(set -o xtrace; jack_connect "$PA_SINK_CLIENT:front-right" "$CALFJACKHOST_CLIENT:eq In #2") & pids+=("$!")

# Inputs
(set -o xtrace; jack_connect "$CALFJACKHOST_CLIENT:eq Out #1" "$JALV_LSP_XOVER_CLIENT:in_l") & pids+=("$!")
(set -o xtrace; jack_connect "$CALFJACKHOST_CLIENT:eq Out #2" "$JALV_LSP_XOVER_CLIENT:in_r") & pids+=("$!")

# Sub-woofer
(set -o xtrace; jack_connect "$JALV_LSP_XOVER_CLIENT:band0l" "$CALFJACKHOST_CLIENT:$SUB_STEREO_NAME In #1") & pids+=("$!")
(set -o xtrace; jack_connect "$JALV_LSP_XOVER_CLIENT:band0r" "$CALFJACKHOST_CLIENT:$SUB_STEREO_NAME In #2") & pids+=("$!")
(set -o xtrace; jack_connect "$CALFJACKHOST_CLIENT:$SUB_STEREO_NAME Out #1" "$HARDWARE_OUT_SUB_L") & pids+=("$!")
(set -o xtrace; jack_connect "$CALFJACKHOST_CLIENT:$SUB_STEREO_NAME Out #2" "$HARDWARE_OUT_SUB_R") & pids+=("$!")

# Mids (everything above subs for `SETUP_TARGET=lh` and MIDS/LOW-MIDS for `SETUP_TARGET=lmh` and `SETUP_TARGET=lmmh`
(set -o xtrace; set-mains-configuration "$MAINS_CONFIGURATION") & pids+=("$!")
(set -o xtrace; jack_connect "$CALFJACKHOST_CLIENT:$MID_STEREO_NAME Out #1" "$HARDWARE_OUT_MID_L") & pids+=("$!")
(set -o xtrace; jack_connect "$CALFJACKHOST_CLIENT:$MID_STEREO_NAME Out #2" "$HARDWARE_OUT_MID_R") & pids+=("$!")

if [[ $SETUP_TARGET == lmh ]]; then
	(set -o xtrace; jack_connect "$JALV_LSP_XOVER_CLIENT:band2l" "$CALFJACKHOST_CLIENT:$HI_STEREO_NAME In #1") & pids+=("$!")
	(set -o xtrace; jack_connect "$JALV_LSP_XOVER_CLIENT:band2r" "$CALFJACKHOST_CLIENT:$HI_STEREO_NAME In #2") & pids+=("$!")
	(set -o xtrace; jack_connect "$CALFJACKHOST_CLIENT:$HI_STEREO_NAME Out #1" "$HARDWARE_OUT_HI_L") & pids+=("$!")
	(set -o xtrace; jack_connect "$CALFJACKHOST_CLIENT:$HI_STEREO_NAME Out #2" "$HARDWARE_OUT_HI_R") & pids+=("$!")
elif [[ $SETUP_TARGET == lmmh ]]; then
	(set -o xtrace; jack_connect "$JALV_LSP_XOVER_CLIENT:band2l" "$CALFJACKHOST_CLIENT:$HIMID_STEREO_NAME In #1") & pids+=("$!")
	(set -o xtrace; jack_connect "$JALV_LSP_XOVER_CLIENT:band2r" "$CALFJACKHOST_CLIENT:$HIMID_STEREO_NAME In #2") & pids+=("$!")
	(set -o xtrace; jack_connect "$CALFJACKHOST_CLIENT:$HIMID_STEREO_NAME Out #1" "$HARDWARE_OUT_HIMID_L") & pids+=("$!")
	(set -o xtrace; jack_connect "$CALFJACKHOST_CLIENT:$HIMID_STEREO_NAME Out #2" "$HARDWARE_OUT_HIMID_R") & pids+=("$!")
	(set -o xtrace; jack_connect "$JALV_LSP_XOVER_CLIENT:band3l" "$CALFJACKHOST_CLIENT:$HI_STEREO_NAME In #1") & pids+=("$!")
	(set -o xtrace; jack_connect "$JALV_LSP_XOVER_CLIENT:band3r" "$CALFJACKHOST_CLIENT:$HI_STEREO_NAME In #2") & pids+=("$!")
	(set -o xtrace; jack_connect "$CALFJACKHOST_CLIENT:$HI_STEREO_NAME Out #1" "$HARDWARE_OUT_HI_L") & pids+=("$!")
	(set -o xtrace; jack_connect "$CALFJACKHOST_CLIENT:$HI_STEREO_NAME Out #2" "$HARDWARE_OUT_HI_R") & pids+=("$!")
fi

>&2 printf 'Waiting for the main JACK ports connectivity PIDs: %s\n' "${pids[*]}"
wait -- "${pids[@]}"
