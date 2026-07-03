#! /usr/bin/env dash
set -o errexit || exit; set -o nounset; set -o pipefail
SCRIPT_DIR=$(dirname -- "$0"); CDPATH='' cd -- "$SCRIPT_DIR"
set -o xtrace
rm -vf -- wenzels-keyboard
