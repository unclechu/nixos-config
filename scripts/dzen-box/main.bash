BG_COLOR=black; FG_DEFAULT_COLOR=white
TEXT=$1; FG_COLOR=$2; [[ -z $FG_COLOR ]] && FG_COLOR=$FG_DEFAULT_COLOR
WM_TITLE='dzen-box'
BUS="/tmp/${WM_TITLE}-u${UID}-d${DISPLAY#:}"
BUS_LOCK="${BUS}.lock"

FONT_FAMILY=Hack; FONT_STYLE=bold

FONT_SIZE=9; ((${#TEXT} <= 2)) && FONT_SIZE=70
             ((${#TEXT} == 3)) && FONT_SIZE=42
             ((${#TEXT} == 4)) && FONT_SIZE=32

font_str() {
  local font_size=$1
  printf -- '-*-%s-%s-*-*-*-%s-*-*-*-*-*-*-*' \
    "$FONT_FAMILY" "$FONT_STYLE" "$font_size"
}

COLORFUL_TEXT="^fn(`font_str "$FONT_SIZE"`)^fg(${FG_COLOR})${TEXT}"

if [[ -f $BUS ]]; then
  inotifywait -qqe delete -- "$BUS_LOCK" 2>/dev/null || true
  >"$BUS_LOCK"

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
X=100 ; (($X<0)) && X=$(($X-$W))
Y=-100 ; (($Y<0)) && Y=$(($Y-$H))

{
  finish() { rm -f -- "$BUS" "$BUS_LOCK"; }
  trap finish EXIT
  printf '%s\n' "$COLORFUL_TEXT" >"$BUS"
  get_size() { printf '%d' "$(stat --printf='%s' -- "$BUS" 2>&-)"; }

  killer() {
    sleep -- "${TIMEOUT_SECONDS}s"
    (( `get_size` == 0 )) && finish || touch -- "$BUS"
  }

  killer & timeout_pid=$!

  while true; do
    [[ ! -f $BUS ]] && exit 1

    while (( `get_size` > 0 )); do
      inotifywait -qqe delete -- "$BUS_LOCK" 2>/dev/null || true
      >"$BUS_LOCK"

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
    -fn "`font_str 9`" 1>&- 2>&-
} 2>/dev/null &

# vim: se et sw=2 ts=2 sts=2 :
