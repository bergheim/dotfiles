#!/bin/sh
BAT=/sys/class/power_supply/BAT0
[ -d "$BAT" ] || exit 0

cap=$(cat "$BAT/capacity" 2>/dev/null)
status=$(cat "$BAT/status" 2>/dev/null)

icons=" 󰁻 󰁽 󰁿 󰂁 󰂃 "
idx=$(( cap / 20 ))
[ "$idx" -gt 5 ] && idx=5
icon=$(printf '%s' "$icons" | awk -v i="$idx" '{print $(i+1)}')

case "$status" in
    Charging|Full) icon="󰃨" ;;
    *) : ;;
esac

class=""
[ "$cap" -le 30 ] && class="warning"
[ "$cap" -le 15 ] && class="critical"

printf '{"text":"%s %s%%","tooltip":"Battery: %s%% (%s)","class":"%s"}\n' \
    "$icon" "$cap" "$cap" "$status" "$class"
