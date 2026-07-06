#! /usr/bin/env bash
set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
SCRIPT_DIR=$(dirname -- "$0"); CDPATH='' cd -- "$SCRIPT_DIR/.." # Project root

# Guard dependencies
>/dev/null type nim
>/dev/null type clunky-toml-json-converter
>/dev/null type jq

ARGS=$(<./config.toml clunky-toml-json-converter toml2json | jq -re '.nim."build-arguments" | .[]')
readarray -t ARGS_LIST <<< "$ARGS"

set -o xtrace
exec nim compile --run "${ARGS_LIST[@]}" \
	--key-repeated-delay=170 \
	--key-repeated-interval=30 \
	--xkb-layout='us,ru,fi' \
	--xkb-options='eurosign:e,grp:shifts_toggle' \
	"$@"
