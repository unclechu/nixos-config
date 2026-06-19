#! /usr/bin/env bash
set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail

CMD=(
	ghc -O0 xmonad.hs
	-o xmonad-x86_64-linux
	# Keep up-to-date with `services.xserver.windowManager.xmonad.ghcArgs`
	-threaded
	-rtsopts
	-with-rtsopts=-N
)

set -o xtrace
"${CMD[@]}" "$@"
