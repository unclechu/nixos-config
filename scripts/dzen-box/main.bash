#! /usr/bin/env bash
# Author: Viacheslav Lotsmanov
# License: MIT https://raw.githubusercontent.com/unclechu/nixos-config/master/LICENSE
set -o errexit || exit

# Note: This is a junky hacky script (subject to refactoring).

# Guard dependencies
>/dev/null type inotifywait
>/dev/null type sed
>/dev/null type dzen2
>/dev/null type rm
>/dev/null type stat
>/dev/null type sleep
>/dev/null type head
>/dev/null type touch

exec <&- # Close stdin

BG_COLOR=black; FG_DEFAULT_COLOR=white
TEXT=$1; FG_COLOR=$2; if [[ -z $FG_COLOR ]]; then FG_COLOR=$FG_DEFAULT_COLOR; fi
WM_TITLE='dzen-box'
BUS="$XDG_RUNTIME_DIR/${WM_TITLE}-d${DISPLAY#:}"
BUS_LOCK="${BUS}.lock"

FONT_FAMILY=Hack; FONT_STYLE=bold

FONT_SIZE=9
if ((${#TEXT} <= 2)); then FONT_SIZE=70; fi
if ((${#TEXT} == 3)); then FONT_SIZE=42; fi
if ((${#TEXT} == 4)); then FONT_SIZE=32; fi

font_str() {
  local font_size=$1
  printf -- '-*-%s-%s-*-*-*-%s-*-*-*-*-*-*-*' \
    "$FONT_FAMILY" "$FONT_STYLE" "$font_size"
}

COLORFUL_TEXT="^fn($(font_str "$FONT_SIZE"))^fg($FG_COLOR)$TEXT"

if [[ -f $BUS ]]; then
  inotifywait -qqe delete -- "$BUS_LOCK" 2>/dev/null || true
  touch -- "$BUS_LOCK"

  if [[ -f $BUS ]]; then
    printf '%s\n' "$COLORFUL_TEXT" >>"$BUS"
  else
    rm -f -- "$BUS_LOCK"
    "$0" "$@" &
    exit 0
  fi

  rm -f -- "$BUS_LOCK"
  exit 0
fi

TIMEOUT_SECONDS=1
W=120
H=120
X=100 ; ((X<0)) && X=$((X-W))
Y=-100 ; ((Y<0)) && Y=$((Y-H))

{
  finish() { rm -f -- "$BUS" "$BUS_LOCK"; }
  trap finish EXIT
  printf '%s\n' "$COLORFUL_TEXT" >"$BUS"
  get_size() { printf '%d' "$(stat --printf='%s' -- "$BUS" 2>&-)"; }

  killer() {
    sleep -- "${TIMEOUT_SECONDS}s"
    if (( "$(get_size)" == 0 )); then finish; fi
    touch -- "$BUS"
  }

  killer & timeout_pid=$!

  while true; do
    if [[ ! -f $BUS ]]; then exit 1; fi

    while (( "$(get_size)" > 0 )); do
      inotifywait -qqe delete -- "$BUS_LOCK" 2>/dev/null || true
      touch -- "$BUS_LOCK"

      head -n 1 -- "$BUS"
      sed -i 1d -- "$BUS"

      kill -- "$timeout_pid" &>/dev/null || true
      killer & timeout_pid=$!

      rm -f -- "$BUS_LOCK"
    done

    inotifywait -qq -- "$BUS" 2>/dev/null
  done | dzen2 \
    -ta c \
    -title-name "$WM_TITLE" \
    -w "$W" -h "$H" -x "$X" -y "$Y" \
    -bg "$BG_COLOR" -fg "$FG_DEFAULT_COLOR" \
    -fn "$(font_str 9)" 1>&- 2>&-
} 2>/dev/null &

# vim: se et sw=2 ts=2 sts=2 :
