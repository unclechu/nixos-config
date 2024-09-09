#! /usr/bin/env bash
set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail

# Build “audio” system profile helper script.
#
# Do not forget to run “sudo nixos-rebuild boot” after you rebuild and apply
# “audio” profile. Because the default profile would be one that is built last.
# For example:
#
#   sudo ./build-audio-profile.sh boot
#   sudo nixos-rebuild boot
#

PROFILE_NAME=audio

# First argument is action (e.g. “build”, “boot”, “switch”).
# Not that there is no point to “switch” from “default” profile since the kernel
# is different. And the main feature of the “audio” profile is realtime kernel.
if (( $# == 0 )); then
	action=build
else
	action=$1
	shift
fi

CMD=(
	nixos-rebuild
	"$action"
	-p "$PROFILE_NAME"
	-I "nixos-config=/etc/nixos/configuration-${PROFILE_NAME}.nix"
)
set -o xtrace
"${CMD[@]}" "$@"
