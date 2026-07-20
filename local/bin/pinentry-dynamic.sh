#!/usr/bin/env bash

# gpg-agent spawns this with the client session's std env. Clients with no
# display env (e.g. the Emacs daemon, which systemd starts before sway) pass
# nothing through — and the agent does NOT fall back to the env stored by
# `updatestartuptty` for env-less clients (verified empirically 2026-07-19).
# So probe for a live Wayland socket ourselves before giving up on a GUI.
if [ -z "$DISPLAY" ] && [ -z "$WAYLAND_DISPLAY" ]; then
    for s in "${XDG_RUNTIME_DIR:-/run/user/$(id -u)}"/wayland-*; do
        case "$s" in *.lock) continue ;; esac
        if [ -S "$s" ]; then
            export WAYLAND_DISPLAY="${s##*/}"
            break
        fi
    done
fi
if [ -n "$DISPLAY" ] || [ -n "$WAYLAND_DISPLAY" ]; then
    exec pinentry-qt "$@"
else
    exec pinentry-curses "$@"
fi
