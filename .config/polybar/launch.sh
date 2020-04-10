#!/usr/bin/env sh

killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

xrdb ~/.Xresources

polybar tb &
polybar tb2 &

echo "Bars launched..."
