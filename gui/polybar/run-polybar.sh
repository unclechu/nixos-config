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
>/dev/null type diff

SCRIPT_DIR=$(dirname -- "${BASH_SOURCE[0]}")
cd -- "$SCRIPT_DIR" # Change working directory to the directory of this script

: "${POLYBAR_CONFIG_FILE:=$PWD/config.ini}"

MONITORS_STR=$(polybar --list-monitors)
readarray -t MONITORS <<< "$MONITORS_STR"

# A mapping between display name (e.g. “HDMI1”) and window title max length.
declare -a MONITOR_NAMES
# Also populating a list of monitor names (preserving the order).
declare -A MONITORS_MAP
for monitor in "${MONITORS[@]}"; do
	name=$(<<<"$monitor" cut -d : -f 1)
	MONITOR_NAMES+=("$name")

	width=$(<<<"$monitor" cut -d : -f 2 | cut -d x -f 1)
	width=${width// /} # Remove spaces at the beginning

	# Polybar can’t auto-shrink taking all available space, you have to limit it by chars amount.
	# Adjust the value to always fit everything including “Caps Lock” indicator.
	# Width divided by approximate char width minus max amount of chars for the other modules.
	# N.B. This is a dirty hack to overcome years old Polybar’s limitations.
	MONITORS_MAP["$name"]=$(( (width / 8) - 113 ))
done

MONITOR_PRIMARY=
PSEUDO_PRIMARY_FILE=$HOME/.pseudo-primary-display
if [[ -f $PSEUDO_PRIMARY_FILE ]]; then
	PSEUDO_PRIMARY_NUM=$(<"$PSEUDO_PRIMARY_FILE")
	echo "$PSEUDO_PRIMARY_NUM"
	for (( n=1; n<="${#MONITOR_NAMES[@]}"; ++n )); do
		if (( n == PSEUDO_PRIMARY_NUM )); then
			MONITOR_PRIMARY=${MONITOR_NAMES[$(( PSEUDO_PRIMARY_NUM - 1 ))]}
			break
		fi
	done
else
	MONITOR_PRIMARY=$(polybar --list-monitors | grep -F '(primary)' | cut -d : -f 1)
fi

>&2 printf 'Picking “%s” display as primary\n' "$MONITOR_PRIMARY"

for hwmon_dir in /sys/devices/platform/coretemp.0/hwmon/hwmon*; do
	if [[ -r "$hwmon_dir/name" ]] \
	&& [[ "$(<"$hwmon_dir/name")" == coretemp ]] \
	&& [[ -r "$hwmon_dir/temp1_input" ]] \
	&& [[ -r "$hwmon_dir/temp1_label" ]]
	then
		if [[ "$(<"$hwmon_dir/temp1_label")" == Package\ id* ]] \
		|| [[ "$(<"$hwmon_dir/temp1_label")" == Physical\ id* ]]
		then
			CPU_TEMP_HWMON_PATH=$hwmon_dir/temp1_input
		fi
	fi
done

POLYBAR_CMD=(polybar --config="$POLYBAR_CONFIG_FILE")

# A hacky detector if backlight is available on this hardware.
#
# Checks if there are backlight controlling files and the builtin display is actually turned on
# (actual_brightness ≠ brightness). When the display is turned off “actual_brightness” is 0.
backlight-module() {
	local BACKLIGHT_DIR=/sys/class/backlight/intel_backlight
	local BRIGHTNESS_FILE=$BACKLIGHT_DIR/brightness
	local ACTUAL_BRIGHTNESS_FILE=$BACKLIGHT_DIR/actual_brightness
	[[ -r $BRIGHTNESS_FILE && -r $ACTUAL_BRIGHTNESS_FILE ]] \
		&& (>/dev/null diff -- "$BRIGHTNESS_FILE" "$ACTUAL_BRIGHTNESS_FILE")
}

# Detect if this hardware has a battery.
battery-module() {
	[[ -e /sys/class/power_supply/BAT0 ]]
}

terminate-old-polybars() {
	# Terminate previous Polybar processes (if there are any)
	(set -o xtrace; polybar-msg cmd quit || true)
}

# Run Polybar on each display and store each Polybar’s PID into “pids” list.
#
# Note that “pids" list is modified outside of scope of this function.
run-per-monitor-polybars() {
	local bar barsub monitor

	# A bar on each display
	for monitor in "${!MONITORS_MAP[@]}"; do
		bar=main
		barsub=
		if [[ $monitor != "$MONITOR_PRIMARY" ]]; then bar=secondary; fi
		if backlight-module; then barsub=${barsub}bl; fi
		if battery-module; then barsub=${barsub}bt; fi
		(
			export MONITOR="$monitor"
			export WINDOW_TITLE_MAX_LEN="${MONITORS_MAP["$monitor"]}"
			if [[ -v CPU_TEMP_HWMON_PATH ]]; then
				export CPU_TEMP_HWMON_PATH
			else
				# Avoid default value. The fallback is not CPU temperature.
				# Can cause confusion if the path is not found.
				# Dummy value just disables the temperature module.
				export CPU_TEMP_HWMON_PATH=--DISABLED--
			fi
			exec -a polybar "${POLYBAR_CMD[@]}" "${bar}${barsub:+-}${barsub}"
		) &
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
		terminate-old-polybars
		run-per-monitor-polybars

		# Listen for any event but OPEN (Polybar triggers it when starting)
		while inotifywait -- "$POLYBAR_CONFIG_FILE" | grep OPEN; do :; done

		kill -- "${pids[@]}" || true
		wait -- "${pids[@]}"
	done
}

# Listen for stdin reports for modules.
#
# IPC reports examples:
#
# ```
# TRIGGER_HOOK xmonad-workspaces 0
# ```
handle-ipc-reports() {
	while read -r line; do
		if [[ $line =~ ^TRIGGER_HOOK[[:space:]]+([^[:space:]]+)[[:space:]]+([0-9]+)$ ]]; then
			module_name=${BASH_REMATCH[1]}
			hook_idx=${BASH_REMATCH[2]}

			notifyPids=()
			for pid in "${polybar_pids[@]}"; do
				(
					set -o xtrace
					</dev/null 1>&2 polybar-msg -p "$pid" action "$module_name" hook "$hook_idx"
				) &
				notifyPids+=($!)
			done
			wait -- "${notifyPids[@]}"
		else
			>&2 printf 'Unrecognized IPC report: “%s”\n' "$line"
		fi
	done
}

# Arguments parsing

# Main action is just to run Polybar
if (( $# == 0 )); then
	terminate-old-polybars
	pids=() # All pids to wait for & cleanup
	cleanup-hook

	>&2 echo '+ run-per-monitor-polybars'
	run-per-monitor-polybars
	polybar_pids=("${pids[@]}") # Only pids of Polybar instances
	>&2 printf 'Polybar pids: %s\n' "${polybar_pids[*]}"

	>&2 echo 'Waiting for IPC reports…'
	</dev/stdin handle-ipc-reports &
	pids+=($!)

	set -o xtrace
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
