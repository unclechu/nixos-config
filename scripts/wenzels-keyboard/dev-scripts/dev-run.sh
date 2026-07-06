#! /usr/bin/env bash
set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
SCRIPT_DIR=$(dirname -- "$0"); CDPATH='' cd -- "$SCRIPT_DIR/.." # Project root

# Guard dependencies
>/dev/null type nim
>/dev/null type clunky-toml-json-converter
>/dev/null type jq

ARGS=$(<./config.toml clunky-toml-json-converter toml2json | jq -re '.nim."build-arguments" | .[]')
readarray -t ARGS_LIST <<< "$ARGS"

CONSTANTS_JSON=$(<../../constants.toml clunky-toml-json-converter toml2json)
KEY_REPEAT_DELAY=$(<<<"$CONSTANTS_JSON" jq -er .keyRepeat.delay)
KEY_REPEAT_INTERVAL=$(<<<"$CONSTANTS_JSON" jq -er .keyRepeat.interval)
XKB_LAYOUT=$(<<<"$CONSTANTS_JSON" jq -er .xkb.layout)
XKB_OPTIONS=$(<<<"$CONSTANTS_JSON" jq -er .xkb.options)

OPTIONS_ARGS=(
	--key-repeated-delay "$KEY_REPEAT_DELAY"
	--key-repeated-interval "$KEY_REPEAT_INTERVAL"
	--xkb-layout "$XKB_LAYOUT"
	--xkb-options "$XKB_OPTIONS"
)

set -o xtrace
exec nim compile --run "${ARGS_LIST[@]}" "${OPTIONS_ARGS[@]}" "$@"
