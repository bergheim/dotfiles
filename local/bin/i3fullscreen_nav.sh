#!/usr/bin/env sh
FULLSCREEN=$(xprop -id $(xdotool getactivewindow) _NET_WM_STATE | grep _NET_WM_STATE_FULLSCREEN)
COMMAND="$@"

[[ -z "${FULLSCREEN}" ]] || COMMAND="fullscreen disable, ${COMMAND}, fullscreen enable"

i3-msg "${COMMAND}"
