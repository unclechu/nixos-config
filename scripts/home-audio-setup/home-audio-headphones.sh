#! /usr/bin/env bash
# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
set -o errexit || exit; set -o nounset; set -o pipefail
SCRIPT_DIR=$(dirname -- "${BASH_SOURCE[0]}"); cd -- "$SCRIPT_DIR"

# Switches between my speakers audio setup and headphones.
#
# Usage example:
#   ./home-audio-headphones.sh
#   ./home-audio-headphones.sh on
#   ./home-audio-headphones.sh off

# Guard dependencies
>/dev/null type jack_control
>/dev/null type pactl
>/dev/null type amp-power-switch

# Environment variable parameters

# Or 'PulseAudio JACK Sink-01'
: "${PA_SINK_CLIENT:=PulseAudio JACK Sink}"

: "${CALFJACKHOST_CLIENT:=home-audio-calf}"

: "${AMP_POWER_CONTROLLER_JACK_WATCH_TRIGGER_PORT:=amp_power_controller_watch:in}"

: "${HEADPHONES_L_PORT:=system:playback_3}"
: "${HEADPHONES_R_PORT:=system:playback_4}"

# Command-line arguments parsing

if (( $# == 0 )); then
	IS_ON=true
elif (( $# == 1 )) && [[ ${1^^} =~ ^(ON|OFF)$ ]]; then
	if [[ ${1^^} == ON ]]; then
		IS_ON=true
	else
		IS_ON=false
	fi
else
	>&2 printf 'Unexpected argument: “%s”\n' "$@"
	>&2 printf 'Usage: %q (ON|OFF)\n' "$0"
	exit 1
fi

pids=()

if $IS_ON; then
	# Disconnect speakers
	(set -o xtrace; jack_disconnect "$PA_SINK_CLIENT:front-left" "$CALFJACKHOST_CLIENT:eq In #1" || :) & pids+=("$!")
	(set -o xtrace; jack_disconnect "$PA_SINK_CLIENT:front-right" "$CALFJACKHOST_CLIENT:eq In #2" || :) & pids+=("$!")
	# Everything that goes to pre-EQ should go to the amp power controller trigger port too
	(set -o xtrace; jack_disconnect "$PA_SINK_CLIENT:front-left" "$AMP_POWER_CONTROLLER_JACK_WATCH_TRIGGER_PORT" || :) & pids+=("$!")
	(set -o xtrace; jack_disconnect "$PA_SINK_CLIENT:front-right" "$AMP_POWER_CONTROLLER_JACK_WATCH_TRIGGER_PORT" || :) & pids+=("$!")
	(set -o xtrace; amp-power-switch off) # Switch off amplifier immediately

	wait -- "${pids[@]}"
	pids=()
	(set -o xtrace; pactl set-sink-volume 'jack_out' 40%) # Set default level for the headphones

	# Connect headphones
	(set -o xtrace; jack_connect "$PA_SINK_CLIENT:front-left" "$HEADPHONES_L_PORT" || :) & pids+=("$!")
	(set -o xtrace; jack_connect "$PA_SINK_CLIENT:front-right" "$HEADPHONES_R_PORT" || :) & pids+=("$!")
else
	# Disconnect headphones
	(set -o xtrace; jack_disconnect "$PA_SINK_CLIENT:front-left" "$HEADPHONES_L_PORT" || :) & pids+=("$!")
	(set -o xtrace; jack_disconnect "$PA_SINK_CLIENT:front-right" "$HEADPHONES_R_PORT" || :) & pids+=("$!")

	wait -- "${pids[@]}"
	pids=()
	(set -o xtrace; pactl set-sink-volume 'jack_out' 25%) # Set default level for the speakers
	(set -o xtrace; amp-power-switch on) # Switch on amplifier immediately

	# Connect speakers
	(set -o xtrace; jack_connect "$PA_SINK_CLIENT:front-left" "$CALFJACKHOST_CLIENT:eq In #1" || :) & pids+=("$!")
	(set -o xtrace; jack_connect "$PA_SINK_CLIENT:front-right" "$CALFJACKHOST_CLIENT:eq In #2" || :) & pids+=("$!")
	# Everything that goes to pre-EQ should go to the amp power controller trigger port too
	(set -o xtrace; jack_connect "$PA_SINK_CLIENT:front-left" "$AMP_POWER_CONTROLLER_JACK_WATCH_TRIGGER_PORT" || :) & pids+=("$!")
	(set -o xtrace; jack_connect "$PA_SINK_CLIENT:front-right" "$AMP_POWER_CONTROLLER_JACK_WATCH_TRIGGER_PORT" || :) & pids+=("$!")
fi

>&2 printf 'Waiting for the main JACK ports connectivity PIDs: %s\n' "${pids[*]}"
wait -- "${pids[@]}"
