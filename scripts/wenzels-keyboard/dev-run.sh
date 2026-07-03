#! /usr/bin/env dash
set -o errexit || exit; set -o nounset; set -o pipefail
SCRIPT_DIR=$(dirname -- "$0"); CDPATH='' cd -- "$SCRIPT_DIR"
set -o xtrace
exec nim compile --run \
	--mm:atomicArc \
	-o:wenzels-keyboard ./wenzels_keyboard.nim \
	--key-repeated-delay=170 \
	--key-repeated-interval=30 \
	--xkb-layout='us,ru,fi' \
	--xkb-options='eurosign:e,grp:shifts_toggle' \
	"$@"
