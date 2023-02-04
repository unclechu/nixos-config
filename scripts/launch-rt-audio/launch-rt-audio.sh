#! /usr/bin/env bash
# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# 1. Switch CPUs to “performance” mode (e.g. from “powersave”)
# 2. Start JACK server
# 3. Set audio buffer size to low-latency for real-time playing with processing

set -o errexit || exit
set -o nounset
set -o pipefail

if (( $# > 1 )) || ( (( $# == 1 )) && [[ ! $1 =~ ^[0-9]+$ ]] ); then
  >&2 echo 'Incorrect arguments!'
  exit 1
fi

cpus=$(printf '%s\n' /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor)
readarray -t cpus <<< "$cpus"

for cpu in "${cpus[@]}"; do
	(set -o xtrace; echo performance | sudo tee -- "$cpu")
done

set -o xtrace
jack_control start
jack_bufsize "${1:-64}"
