#! /usr/bin/env bash
# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE

# Render KiCad schematic printed to a PDF file into a PNG file.
#
# Just a simple preset for ImageMagick parameters.

set -o errexit || exit; set -o nounset; set -o pipefail

# Guarding dependencies
>/dev/null type gs # Ghostscript dependency to be able to convert PDF to PNG
>/dev/null type magick
>/dev/null type basename

if (( $# != 1 )) || [[ -z $1 ]]; then
	>&2 echo 'Usage:'
	>&2 printf "  %s file.pdf\n" "${0%Q}"
	exit 1
fi

set -o xtrace

# This will also cut off the directory path.
# The PNG file is always saved to the current working directory.
FILE_NAME_WITHOUT_PDF_EXT=$(basename -- "$1" .pdf)
magick -density 300 "$1" -resize 50% "${FILE_NAME_WITHOUT_PDF_EXT}".png
