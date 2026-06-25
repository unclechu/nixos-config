#! /usr/bin/env bash
set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail

# Build specialized system profile (e.g. “audio" or “graphics") helper script.
#
# Do not forget to run “sudo nixos-rebuild boot” after you rebuild and apply
# “audio” profile. Because the default profile would be one that is built last.
# For example:
#
#   sudo ./build-profile.sh audio boot
#   sudo ./build-profile.sh graphics boot
#   sudo nixos-rebuild boot
#

PROFILE_NAME=audio
# Parsing profile argument
if (( $# < 1 )); then
	>&2 echo 'System profile is not specified!'
	exit 1
else
	PROFILE_NAME=$1; shift
fi

NIXOS_PROFILE_CONFIG=/etc/nixos/configuration-${PROFILE_NAME}.nix

if [[ ! -f "$NIXOS_PROFILE_CONFIG" ]]; then
	>&2 printf \
		'NixOS configuration “%s” is not found for “%s” profile!\n' \
		"$NIXOS_PROFILE_CONFIG" "$PROFILE_NAME"
	exit 1
fi

# Defaulting action argument (e.g. “build”, “boot”, “switch”)
# if not explicitly provided.
if (( $# == 0 )); then
	ACTION=build
else
	ACTION=$1
	shift
fi

CMD=(
	nixos-rebuild
	"$ACTION"
	-p "$PROFILE_NAME"
	-I "nixos-config=$NIXOS_PROFILE_CONFIG"
)
set -o xtrace
"${CMD[@]}" "$@"
