#! /usr/bin/env bash

# Polybar runner script.
#
# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

set -o errexit || exit
set -o nounset
set -o pipefail

# Guard dependencies
>/dev/null type polybar
>/dev/null type polybar-msg
>/dev/null type inotifywait
>/dev/null type grep
>/dev/null type cut

SCRIPT_DIR=$(dirname -- "${BASH_SOURCE[0]}")
cd -- "$SCRIPT_DIR" # Change working directory to the directory of this script

CONFIG_FILE=$PWD/config.ini

MONITOR_PRIMARY=$(polybar --list-monitors | grep -F '(primary)' | cut -d ':' -f 1)
MONITORS_STR=$(polybar --list-monitors | cut -d ':' -f 1)
readarray -t MONITORS <<< "$MONITORS_STR"

POLYBAR_CMD=(polybar --config="$CONFIG_FILE")

watch-reload() (
	set -o errexit || exit
	set -o nounset
	set -o pipefail

	>&2 printf \
		'Running Polybar in auto-reload mode and watching for changes in the %s config file…\n' \
		"${CONFIG_FILE@Q}"

	pids=()

	# Terminate Polybar when the script is interrupted
	trap 'if (( ${#pids[@]} > 0 )); then kill -- "${pids[@]}" || true; wait -- "${pids[@]}"; fi' EXIT

	set -o xtrace
	while true; do
		pids=()

		# A bar on each display
		for monitor in "${MONITORS[@]}"; do
			bar=main
			if [[ $monitor != "$MONITOR_PRIMARY" ]]; then bar=secondary; fi
			MONITOR="$monitor" "${POLYBAR_CMD[@]}" "$bar" &
			pids+=($!)
		done

		# Listen for any event but OPEN (Polybar triggers it when starting)
		while inotifywait -- "$CONFIG_FILE" | grep OPEN; do :; done

		kill -- "${pids[@]}" || true
		wait -- "${pids[@]}"
	done
)

# Arguments parsing

# Main action is just to run Polybar
if (( $# == 0 )); then
	# Terminate previous Polybar processes (if there are any)
	polybar-msg cmd quit || true

	pids=()

	# Terminate Polybar when the script is interrupted
	trap 'if (( ${#pids[@]} > 0 )); then kill -- "${pids[@]}" || true; wait -- "${pids[@]}"; fi' EXIT

	# A bar on each display
	for monitor in "${MONITORS[@]}"; do
		bar=main
		if [[ $monitor != "$MONITOR_PRIMARY" ]]; then bar=secondary; fi
		MONITOR="$monitor" "${POLYBAR_CMD[@]}" "$bar" &
		pids+=($!)
	done

	wait -- "${pids[@]}"

elif (( $# == 1 )) && [[ $1 == watch ]]; then
	# Terminate previous Polybar processes (if there are any)
	polybar-msg cmd quit || true

	watch-reload

else
	>&2 printf 'Unrecognized argument(s):'
	>&2 printf ' %q' "$@"
	>&2 echo
	exit 1
fi
