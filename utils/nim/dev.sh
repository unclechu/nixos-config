#! /usr/bin/env bash
# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
SCRIPT_DIR=$(dirname -- "${BASH_SOURCE[0]}")
CDPATH='' cd -- "$SCRIPT_DIR" # Expecting project root

# Common dev operations for a Nim project.
#
# This file is supposed to be symlinked to the root of the project.
#
# It relies on a presence of `config.toml` file in the project root with these
# paths:
#
#   - `.nim.main-program` — `string`
#   - `.nim.main-source` — `string`

# Guard dependencies
>/dev/null type nim
>/dev/null type clunky-toml-json-converter
>/dev/null type jq
>/dev/null type rm

if (( $# == 0 )); then
	>&2 echo '[FAIL] This script expects a sub-command'
	exit 1
fi

SUB_COMMAND=$1; shift

CONFIG_JSON=$(<./config.toml clunky-toml-json-converter toml2json)
MAIN_PROGRAM=$(<<<"$CONFIG_JSON" jq -re '.nim."main-program"')
MAIN_SOURCE=$(<<<"$CONFIG_JSON" jq -re '.nim."main-source"')

case "$SUB_COMMAND" in
	clean)
		(
			set -o xtrace
			exec rm -vf -- "$MAIN_PROGRAM"
		)
		;;
	lint)
		(
			set -o xtrace
			exec nim check "$MAIN_SOURCE" "$@"
		)
		;;
	build)
		(
			set -o xtrace
			exec nim compile "-o:$MAIN_PROGRAM" "$MAIN_SOURCE" "$@"
		)
		;;
	run)
		(
			set -o xtrace
			exec nim compile --run "-o:$MAIN_PROGRAM" "$MAIN_SOURCE" "$@"
		)
		;;
	*)
		>&2 printf '[FAIL] Unrecognized sub-command: “%s”\n' "$SUB_COMMAND"
		exit
esac
