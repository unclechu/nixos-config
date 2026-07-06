#! /usr/bin/env bash
set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
SCRIPT_DIR=$(dirname -- "$0"); CDPATH='' cd -- "$SCRIPT_DIR/.." # Project root
set -o xtrace
rm -vf -- wenzels-keyboard
