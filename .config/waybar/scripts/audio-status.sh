#!/bin/sh
# Print current audio sink as waybar JSON.

SPEAKERS="alsa_output.pci-0000_00_1f.3.analog-stereo"
# Hardware game sink (virtual Arctis_Game sink gone since arctis-manager was disabled)
HEADSET="alsa_output.usb-SteelSeries_SteelSeries_Arctis_7-00.stereo-game"

current=$(pactl get-default-sink 2>/dev/null)

case "$current" in
    "$HEADSET")   text="󰋎"; tip="Headset (Arctis 7)" ;;
    "$SPEAKERS")  text="󰓃"; tip="Built-in Speakers" ;;
    *)            text="󰓃"; tip="Sink: $current" ;;
esac

printf '{"text":"%s","tooltip":"%s"}\n' "$text" "$tip"
