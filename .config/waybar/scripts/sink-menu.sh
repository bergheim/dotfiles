#!/bin/sh
# ~/.config/waybar/scripts/sink-menu.sh
# Cycle the default audio output (sink) to the next one, using wireplumber.
block=$(wpctl status | sed -n '/Sinks:/,/Sources:/p')

ids=$(printf '%s\n' "$block" | grep -oP '\d+(?=\.\s)')
[ -z "$ids" ] && exit 0

cur=$(printf '%s\n' "$block" | grep -oP '\*\s+\K\d+(?=\.)')

# next id after current, wrapping to the first
next=$(printf '%s\n' "$ids" | awk -v c="$cur" '
  {a[NR]=$1} END{
    for(i=1;i<=NR;i++) if(a[i]==c){print a[i%NR+1]; found=1}
    if(!found) print a[1]
  }')

wpctl set-default "$next"
