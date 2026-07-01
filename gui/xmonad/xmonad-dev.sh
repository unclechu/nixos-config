#! /usr/bin/env bash
set -o errexit || exit; set -o errtrace; set -o nounset; set -o pipefail

# Guard dependencies
>/dev/null type dirname
>/dev/null type realpath
>/dev/null type clunky-toml-json-converter
>/dev/null type jq
>/dev/null type perl

SCRIPT_DIR=$(dirname -- "${BASH_SOURCE[0]}")
cd -- "$SCRIPT_DIR"
PWD=$(pwd)

XMONAD_EXE=$PWD/xmonad

POLYBAR_RUN_SCRIPT=$(realpath -- "$PWD/../polybar/run-polybar.sh")
export POLYBAR_RUN_SCRIPT

WM_CONFIG_JSON=$(<../wm-config.toml clunky-toml-json-converter toml2json)

TERMINAL=$(<<<"$WM_CONFIG_JSON" jq -re .terminal)
TERMINAL_SH_JSON=$(<<<"$WM_CONFIG_JSON" jq -ce --arg k "$TERMINAL" '."terminal-configuration".[$k]."shell-commands"')
TERMINAL_SH_NEW=$(<<<"$TERMINAL_SH_JSON" jq -re .new)
TERMINAL_SH_ATTACH=$(<<<"$TERMINAL_SH_JSON" jq -re .attach)
TERMINAL_SH_NUKE=$(<<<"$TERMINAL_SH_JSON" jq -re .nuke)
TERMINAL_SH_NEW_PROMPT=$(<<<"$TERMINAL_SH_JSON" jq -re '."new-prompt"')

f() { X="$TERMINAL_SH_NEW" perl -pe 's/%TERMINAL_NEW%/$ENV{X}/'; }

RUNNER=$(<<<"$WM_CONFIG_JSON" jq -re .runner)
RUNNER_SH_JSON=$(<<<"$WM_CONFIG_JSON" jq -ce --arg k "$RUNNER" '."runner-configuration".[$k]."shell-commands"')
RUNNER_SH_RUN_COMMAND=$(<<<"$RUNNER_SH_JSON" jq -re '."run-command"' | f)
RUNNER_SH_RUN_APPLICATION=$(<<<"$RUNNER_SH_JSON" jq -re '."run-application"' | f)
RUNNER_SH_SELECT_WINDOW=$(<<<"$RUNNER_SH_JSON" jq -re '."select-window"' | f)
RUNNER_SH_SELECT_SINGLE_OPTION=$(<<<"$RUNNER_SH_JSON" jq -re '."select-single-option"' | f)

MUSIC_PLAYER=$(<<<"$WM_CONFIG_JSON" jq -re '."music-player"')
MUSIC_PLAYER_SH_JSON=$(<<<"$WM_CONFIG_JSON" jq -ce --arg k "$MUSIC_PLAYER" '."music-player-configuration".[$k]."shell-commands"')
MUSIC_PLAYER_SH_PLAY=$(<<<"$MUSIC_PLAYER_SH_JSON" jq -re '."play"' | f)
MUSIC_PLAYER_SH_PLAY_TOGGLE=$(<<<"$MUSIC_PLAYER_SH_JSON" jq -re '."play-toggle"' | f)
MUSIC_PLAYER_SH_PREVIOUS=$(<<<"$MUSIC_PLAYER_SH_JSON" jq -re '."previous"' | f)
MUSIC_PLAYER_SH_NEXT=$(<<<"$MUSIC_PLAYER_SH_JSON" jq -re '."next"' | f)
MUSIC_PLAYER_SH_STOP=$(<<<"$MUSIC_PLAYER_SH_JSON" jq -re '."stop"' | f)
MUSIC_PLAYER_SH_SPAWN_SERVER=$(<<<"$MUSIC_PLAYER_SH_JSON" jq -re '."spawn-server"' | f)

# WARNING! Keep consistent with `gui/xmonad/default.nix` and `gui/xmonad/xmonad.hs`.
CUSTOM_CONFIGURATION_ARGS=(
  --xmonadrc-terminal-command-new="$TERMINAL_SH_NEW"
  --xmonadrc-terminal-command-attach="$TERMINAL_SH_ATTACH"
  --xmonadrc-terminal-command-nuke="$TERMINAL_SH_NUKE"
  --xmonadrc-terminal-command-new-prompt="$TERMINAL_SH_NEW_PROMPT"

  --xmonadrc-runner-command-run-cmd="$RUNNER_SH_RUN_COMMAND"
  --xmonadrc-runner-command-run-app="$RUNNER_SH_RUN_APPLICATION"
  --xmonadrc-runner-command-select-window="$RUNNER_SH_SELECT_WINDOW"
  --xmonadrc-runner-command-select-single-option="$RUNNER_SH_SELECT_SINGLE_OPTION"

  --xmonadrc-music-player-control-command-play="$MUSIC_PLAYER_SH_PLAY"
  --xmonadrc-music-player-control-command-play-toggle="$MUSIC_PLAYER_SH_PLAY_TOGGLE"
  --xmonadrc-music-player-control-command-prev="$MUSIC_PLAYER_SH_PREVIOUS"
  --xmonadrc-music-player-control-command-next="$MUSIC_PLAYER_SH_NEXT"
  --xmonadrc-music-player-control-command-stop="$MUSIC_PLAYER_SH_STOP"
  --xmonadrc-music-player-control-command-spawn-server="$MUSIC_PLAYER_SH_SPAWN_SERVER"
)

printf '%s\n' "${CUSTOM_CONFIGURATION_ARGS[@]}"

cd -- "$HOME"

# Guard runtime dependencies
>/dev/null type -- "$POLYBAR_RUN_SCRIPT"
>/dev/null type -- "$XMONAD_EXE"

set -o xtrace
XMONAD_DEV=1 exec "$XMONAD_EXE" "${CUSTOM_CONFIGURATION_ARGS[@]}" "$@"
