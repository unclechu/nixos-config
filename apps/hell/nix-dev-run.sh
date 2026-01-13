#! /usr/bin/env bash
set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail

SCRIPT_DIR=$(dirname -- "${BASH_SOURCE[0]}")
cd -- "$SCRIPT_DIR"

set -o xtrace
nix-shell --run './dev-run.sh'
