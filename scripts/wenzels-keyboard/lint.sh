#! /usr/bin/env dash
set -o errexit || exit; set -o nounset; set -o pipefail
SCRIPT_DIR=$(dirname -- "$0"); CDPATH='' cd -- "$SCRIPT_DIR"
set -o xtrace
exec nim check \
	--colors:off --styleCheck:error --hintAsError:XDeclaredButNotUsed:on \
	./wenzels_keyboard.nim "$@"
