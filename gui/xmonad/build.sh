#! /usr/bin/env bash
set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
SCRIPT_DIR=$(dirname -- "${BASH_SOURCE[0]}"); cd -- "$SCRIPT_DIR"

# Guard dependencies
>/dev/null type clunky-toml-json-converter
>/dev/null type jq
>/dev/null type ghc

GHC_ARGS=$(<./shared-config.toml clunky-toml-json-converter toml2json | jq -re '."ghc-arguments" | .[]')
readarray -t GHC_ARGS_LIST <<< "$GHC_ARGS"

CMD=(ghc -O0 xmonad.hs -o xmonad "${GHC_ARGS_LIST[@]}")
set -o xtrace
"${CMD[@]}" "$@"
