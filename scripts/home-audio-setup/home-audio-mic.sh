#! /usr/bin/env bash
# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
set -o errexit || exit; set -o nounset; set -o pipefail
SCRIPT_DIR=$(dirname -- "${BASH_SOURCE[0]}"); cd -- "$SCRIPT_DIR"

# My home audio microphone setup.
#
# Just boosting and limiting the signal.
# One limiter is boosting and limiting peaks loud peaks
# and second limiter makes sure it does not exceed -0.3dB.
#
# Usage example:
#   ./home-audio-mic.sh

# Guard dependencies
>/dev/null type jack_control
>/dev/null type sleep
>/dev/null type pactl

# Environment variable parameters

# Or 'PulseAudio JACK Source-01'
: "${PA_SOURCE_CLIENT:=PulseAudio JACK Source}"

: "${CALFJACKHOST_PRESET:=presets/calfjackhost-mic.xml}"
: "${CALFJACKHOST_CLIENT:=home-audio-mic}"

# Command-line arguments parsing

if (( $# != 0 )); then
	>&2 echo 'This script does not take any arguments!'
	>&2 printf 'Unexpected argument: “%s”\n' "$@"
	exit 1
fi

# Setting everything up

(
	set -o xtrace

	# Launch the JACK server (in case it’s not already started yet)
	jack_control start
	sleep 1s
)

# Start calfjackhost
(
	CMD=(
		calfjackhost
		-c "$CALFJACKHOST_CLIENT"
		-l "$CALFJACKHOST_PRESET"
	)
	set -o xtrace
	"${CMD[@]}" & disown
)

PORTS=(
	"$CALFJACKHOST_CLIENT:Mic Limiter 1 In #1"
	"$CALFJACKHOST_CLIENT:Mic Limiter 1 In #2"
	"$CALFJACKHOST_CLIENT:Mic Limiter 1 Out #1"
	"$CALFJACKHOST_CLIENT:Mic Limiter 1 Out #2"
	"$CALFJACKHOST_CLIENT:Mic Limiter 2 In #1"
	"$CALFJACKHOST_CLIENT:Mic Limiter 2 In #2"
	"$CALFJACKHOST_CLIENT:Mic Limiter 2 Out #1"
	"$CALFJACKHOST_CLIENT:Mic Limiter 2 Out #2"
)

# Waiting for ports readiness
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

set -o xtrace

sleep 1s

# Route the hardware microphone to calfjackhost
jack_connect system:capture_1 "$CALFJACKHOST_CLIENT:Mic Limiter 1 In #1"
jack_connect system:capture_1 "$CALFJACKHOST_CLIENT:Mic Limiter 1 In #2"
jack_connect "$CALFJACKHOST_CLIENT:Mic Limiter 1 Out #1" "$CALFJACKHOST_CLIENT:Mic Limiter 2 In #1"
jack_connect "$CALFJACKHOST_CLIENT:Mic Limiter 1 Out #2" "$CALFJACKHOST_CLIENT:Mic Limiter 2 In #2"

# Remove straight connection from hardware to PulseAudio
jack_disconnect system:capture_1 "$PA_SOURCE_CLIENT:front-left" || true
jack_disconnect system:capture_2 "$PA_SOURCE_CLIENT:front-right" || true

# Finally connect the processed microphone signal to PulseAudio
jack_connect "$CALFJACKHOST_CLIENT:Mic Limiter 2 Out #1" "$PA_SOURCE_CLIENT:front-left"
jack_connect "$CALFJACKHOST_CLIENT:Mic Limiter 2 Out #2" "$PA_SOURCE_CLIENT:front-right"

# Make sure that the JACK PulseAudio input is not attenuated
pactl set-source-volume 'jack_in' 100%
