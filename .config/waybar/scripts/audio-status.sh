#!/bin/sh
# Print current audio sink as waybar JSON.

SPEAKERS="alsa_output.pci-0000_00_1f.3.analog-stereo"
HEADSET="Arctis_Game"

current=$(pactl get-default-sink 2>/dev/null)

case "$current" in
    "$HEADSET")   text="󰋎"; tip="Headset (Arctis 7)" ;;
    "$SPEAKERS")  text="󰓃"; tip="Built-in Speakers" ;;
    *)            text="󰓃"; tip="Sink: $current" ;;
esac

printf '{"text":"%s","tooltip":"%s"}\n' "$text" "$tip"
