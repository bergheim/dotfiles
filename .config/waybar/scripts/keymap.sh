#!/bin/sh
# Keyboard-layout indicator, compositor-aware (sway + niri).
# No args: print a flag for the active layout.
# `toggle` arg: cycle to the next layout.
#
# Dual-support rationale: the user alternates sway/niri sessions and
# waybar uses one shared config. Detecting the compositor here (rather
# than forking the config) keeps a single source of truth and stops
# the previous swaymsg-every-second spam under niri.

layout=""

if [ -n "${NIRI_SOCKET:-}" ] && command -v niri >/dev/null 2>&1; then
    if [ "${1:-}" = toggle ]; then
        # If this action name differs on your niri version, layout
        # switching is also bound to Super+Space (xkb grp toggle).
        niri msg action switch-layout next >/dev/null 2>&1
        exit 0
    fi
    layout=$(niri msg --json keyboard-layouts 2>/dev/null \
        | jq -r '.names[.current_idx] // empty')
elif [ -n "${SWAYSOCK:-}" ] && command -v swaymsg >/dev/null 2>&1; then
    if [ "${1:-}" = toggle ]; then
        swaymsg input type:keyboard xkb_switch_layout next >/dev/null 2>&1
        exit 0
    fi
    layout=$(swaymsg -t get_inputs 2>/dev/null \
        | jq -r '[.[] | select(.type=="keyboard")
                  | .xkb_active_layout_name] | first // empty')
fi

case "$layout" in
    *Norwegian*|*Norsk*) printf '%s\n' "🇳🇴" ;;
    "")                  printf '\n' ;;          # no compositor / unknown: blank, no error
    *)                   printf '%s\n' "🇺🇸" ;;
esac
