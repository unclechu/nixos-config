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
exec nim compile "${ARGS_LIST[@]}" "$@"
