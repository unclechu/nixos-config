#! /usr/bin/env bash
set -o errexit || exit; set -o nounset; set -o pipefail
exec <&- # Close standard input

# An example of running this script from shell directly:
#
#   ./run-picom.sh conf/with-vsync.conf
#

# Guard dependencies
>/dev/null type dirname
>/dev/null type picom
>/dev/null type sh # For Feh run
SCRIPT_DIR=$(dirname -- "${BASH_SOURCE[0]}")
[[ -x $SCRIPT_DIR/no-picom.sh || -x $SCRIPT_DIR/no-picom ]] \
	|| (set -o xtrace; [[ -x $SCRIPT_DIR/no-picom.sh || -x $SCRIPT_DIR/no-picom ]])

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
if [[ -x $SCRIPT_DIR/no-picom.sh ]]; then
	(set -o xtrace; "$SCRIPT_DIR/no-picom.sh")
elif [[ -x $SCRIPT_DIR/no-picom ]]; then
	(set -o xtrace; "$SCRIPT_DIR/no-picom")
fi

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
