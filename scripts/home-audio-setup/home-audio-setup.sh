#! /usr/bin/env bash
# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
set -o errexit || exit; set -o nounset; set -o pipefail
SCRIPT_DIR=$(dirname -- "${BASH_SOURCE[0]}"); cd -- "$SCRIPT_DIR"

# My home audio setup script.
#
# Usage example:
#   ./home-audio-setup.sh

# Guard dependencies
>/dev/null type jack_control
>/dev/null type sleep
>/dev/null type pactl

# Command-line arguments parsing

if (( $# != 0 )); then
	>&2 echo 'This script does not take any arguments!'
	>&2 printf 'Unexpected argument: “%s”\n' "$@"
	exit 1
fi

# Setting everything up

set -o xtrace

# Launch the JACK server (in case it’s not already started yet)
jack_control start
sleep 1s

# Start the cross-over setup and connect everything
./home-audio-lh-xover.sh

# Make sure that the JACK PulseAudio input is not attenuated
pactl set-source-volume 'jack_in' 100%
