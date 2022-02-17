#! /usr/bin/env bash
set -Eeuo pipefail || exit

PROFILE_NAME=audio

if (( $# == 0 )); then
	action=build
else
	action=$1
	shift
fi

sudo nixos-rebuild \
	"$action" \
	-p "$PROFILE_NAME" \
	-I "nixos-config=/etc/nixos/configuration-${PROFILE_NAME}.nix" \
	"$@"

if [[ $action == boot || $action == switch ]] && (( $# == 0 )); then
	>&2 echo Making the default system profile be the default one to boot
	# This “audio” profile should not be the default one.
	# So let’s make the “default” profile the default one in GRUB.
	# I’ve been told in #nix:nixos.org that at the moment it’s impossible to build and make it
	# bootable without making it a default selected option in GBUB. So this kind of a hack.
	sudo nixos-rebuild boot
fi
