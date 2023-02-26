#! /usr/bin/env bash
# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# 1. Switch CPUs to “performance” mode (e.g. from “powersave”)
# 2. Start JACK server
# 3. Set audio buffer size to low-latency for real-time playing with processing

set -o errexit || exit
set -o nounset
set -o pipefail

action=on
bufsize=default # Default for “on” is 64, for “off” is 512

for arg in "$@"; do
	if [[ $arg =~ ^[0-9]+$ ]]; then
		bufsize=$arg
	elif [[ $arg =~ ^(on|off)$ ]]; then
		action=$arg
	else
		>&2 printf 'Incorrect argument: "%s"' "$arg"
		exit 1
	fi
done

if [[ $action == on ]]; then
	>&2 echo 'Preparing system for working with real-time audio…'
	if [[ $bufsize == default ]]; then bufsize=64; fi
else
	>&2 echo 'Easing real-time audio mode…'
	if [[ $bufsize == default ]]; then bufsize=512; fi
fi

cpus=$(printf '%s\n' /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor)
readarray -t cpus <<< "$cpus"

mode=$(if [[ $action == on ]]; then echo performance; else echo powersave; fi)

for cpu in "${cpus[@]}"; do
	(set -o xtrace; printf '%s\n' "$mode" | sudo tee -- "$cpu")
done

set -o xtrace
jack_control start
jack_bufsize "${bufsize}"
