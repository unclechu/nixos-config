#! /usr/bin/env bash
set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
exec <&- # Close standard input

# An example of running this script from shell directly:
#
#   ./run-picom.sh conf/with-vsync.conf
#

# Guard dependencies
>/dev/null type dirname
>/dev/null type picom
>/dev/null type sh # For Feh run

# Runtime environment dependencies guarding
if [[ ! -v NO_PICOM_SCRIPT_EXE ]]; then
	SCRIPT_DIR=$(dirname -- "${BASH_SOURCE[0]}")
	NO_PICOM_SCRIPT_EXE=$SCRIPT_DIR/no-picom.sh
fi
>/dev/null type -- "$NO_PICOM_SCRIPT_EXE"

# Arguments parsing
if (( $# == 1 )) && [[ -f $1 && -r $1 ]]; then
	PICOM_CONFIG_FILE=$1; shift

elif (( $# == 0 )) \
&& [[ -v DEFAULT_PICOM_CONFIG_FILE ]] \
&& [[ -f $DEFAULT_PICOM_CONFIG_FILE ]] \
&& [[ -r $DEFAULT_PICOM_CONFIG_FILE ]]
then
	PICOM_CONFIG_FILE=$DEFAULT_PICOM_CONFIG_FILE
else
	>&2 printf '“run-picom” script takes exactly one argument '
	>&2 printf 'with the Picom configuration file!\n'
	exit 1
fi

# Kill previous Picom run first
(set -o xtrace; "$NO_PICOM_SCRIPT_EXE")

# Run Picom in background as a daemon
picom --config "$PICOM_CONFIG_FILE" & disown

# Update Feh-managed wallpaper
if [[ -f ~/.fehbg ]]; then
	if [[ -x ~/.fehbg ]]; then
		(set -o xtrace; ~/.fehbg & disown)
	else
		(set -o xtrace; sh ~/.fehbg & disown)
	fi
fi
