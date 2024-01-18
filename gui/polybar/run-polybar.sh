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

: "${POLYBAR_CONFIG_FILE:=$PWD/config.ini}"

MONITOR_PRIMARY=$(polybar --list-monitors | grep -F '(primary)' | cut -d : -f 1)
MONITORS_STR=$(polybar --list-monitors)
readarray -t MONITORS <<< "$MONITORS_STR"

# A mapping between display name (e.g. “HDMI1”) and window title max length.
declare -A MONITORS_MAP
for monitor in "${MONITORS[@]}"; do
	name=$(<<<"$monitor" cut -d : -f 1)
	width=$(<<<"$monitor" cut -d : -f 2 | cut -d x -f 1)
	width=${width// /} # Remove spaces at the beginning

	# Polybar can’t auto-shrink taking all available space, you have to limit it by chars amount.
	# Adjust the value to always fit everything including “Caps Lock” indicator.
	# Width divided by approximate char width minus max amount of chars for the other modules.
	# N.B. This is a dirty hack to overcome years old Polybar’s limitations.
	MONITORS_MAP["$name"]=$(( (width / 8) - 111 ))
done

POLYBAR_CMD=(polybar --config="$POLYBAR_CONFIG_FILE")

# Run Polybar on each display and store each Polybar’s PID into “pids” list.
#
# Note that “pids" list is modified outside of scope of this function.
run-per-monitor-polybars() {
	# A bar on each display
	for monitor in "${!MONITORS_MAP[@]}"; do
		bar=main
		if [[ $monitor != "$MONITOR_PRIMARY" ]]; then bar=secondary; fi
		MONITOR="$monitor" \
			WINDOW_TITLE_MAX_LEN="${MONITORS_MAP["$monitor"]}" \
			"${POLYBAR_CMD[@]}" \
			"$bar" &
		pids+=($!)
	done
}

cleanup-hook() {
	# Terminate Polybar when the script is interrupted
	trap '
		if (( ${#pids[@]} > 0 )); then
			kill -- "${pids[@]}" || true
			wait -- "${pids[@]}"
		fi
	' EXIT
}

watch-reload() {
	>&2 printf \
		'Running Polybar in auto-reload mode and watching for changes in the %s config file…\n' \
		"${POLYBAR_CONFIG_FILE@Q}"

	pids=()
	cleanup-hook

	set -o xtrace
	while true; do
		pids=()
		run-per-monitor-polybars

		# Listen for any event but OPEN (Polybar triggers it when starting)
		while inotifywait -- "$POLYBAR_CONFIG_FILE" | grep OPEN; do :; done

		kill -- "${pids[@]}" || true
		wait -- "${pids[@]}"
	done
}

terminate-old-polybars() {
	# Terminate previous Polybar processes (if there are any)
	(set -o xtrace; polybar-msg cmd quit || true)
}

# Arguments parsing

# Main action is just to run Polybar
if (( $# == 0 )); then
	terminate-old-polybars
	pids=()
	cleanup-hook

	set -o xtrace
	run-per-monitor-polybars
	wait -- "${pids[@]}"

elif (( $# == 1 )) && [[ $1 == watch ]]; then
	terminate-old-polybars
	watch-reload

else
	>&2 printf 'Unrecognized argument(s):'
	>&2 printf ' %q' "$@"
	>&2 echo
	exit 1
fi
