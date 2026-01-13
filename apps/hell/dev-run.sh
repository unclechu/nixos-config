#! /usr/bin/env bash
set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail

SCRIPT_DIR=$(dirname -- "${BASH_SOURCE[0]}")
cd -- "$SCRIPT_DIR"

set -o xtrace
exec ghci -ignore-dot-ghci -ghci-script <(
	cat -- "$PWD/config.hs"
	echo ':set prompt "λ "'
	if (( UID == 0 )); then color=31; else color=32; fi
	printf ':set prompt "%s"\n' '\ESC['"$color"'m\STXλ\ESC[m\STX '
)
