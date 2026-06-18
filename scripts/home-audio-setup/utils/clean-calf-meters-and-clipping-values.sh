#! /usr/bin/env bash
# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
set -o errexit || exit; set -o nounset; set -o pipefail
SCRIPT_DIR=$(dirname -- "${BASH_SOURCE[0]}"); cd -- "$SCRIPT_DIR/../presets"

# Remove useless meters and clipping indicator values from Calf XML presets.
#
# Usage example:
#   utils/clean-calf-meters-and-clipping-values.sh

# Guard dependencies
>/dev/null type git
>/dev/null type file
>/dev/null type rg
>/dev/null type cut
>/dev/null type sed

# Command-line arguments parsing

if (( $# != 0 )); then
	>&2 echo 'This script does not take any arguments!'
	>&2 printf 'Unexpected argument: “%s”\n' "$@"
	exit 1
fi

# Cleaning up XML files

# All XML files (all of them are supposed to be Calf presets)
XML_FILES=$(
	git ls-files \
		| while read -r x; do file -- "$x"; done \
		| rg -F ' XML ' \
		| cut -d ':' -f 1
)
readarray -t XML_FILES_ARR <<<"$XML_FILES"

for xml_file in "${XML_FILES_ARR[@]}"; do
	(set -o xtrace; sed -ri '/<param name="(meter|clip)_(in|out)(L|R)" /d' "$xml_file")
done
