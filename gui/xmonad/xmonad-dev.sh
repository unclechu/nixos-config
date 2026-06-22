#! /usr/bin/env bash
set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail
SCRIPT_DIR=$(dirname -- "${BASH_SOURCE[0]}"); cd -- "$SCRIPT_DIR"
PWD=$(pwd)
POLYBAR_RUN_SCRIPT=$(realpath -- "$PWD/../polybar/run-polybar.sh")
XMONAD_EXE=$PWD/xmonad
export POLYBAR_RUN_SCRIPT
export XMONAD_DEV=1
cd -- "$HOME"
exec -a xmonad "$XMONAD_EXE" "$@"
