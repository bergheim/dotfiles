#!/bin/sh

# When run with an argument, toggle the service
if [ "$1" = "toggle" ]; then
    if systemctl --user is-active gammastep >/dev/null; then
        systemctl --user stop gammastep
        notify-send "Night light off"
    else
        systemctl --user start gammastep
        notify-send "Night light on"
    fi
    exit
fi

# Otherwise just report the status
if systemctl --user is-active gammastep >/dev/null; then
    echo '{"text": "", "tooltip": "Night light on", "class": "on", "alt": "on"}'
else
    echo '{"text": "", "tooltip": "Night light off", "class": "off", "alt": "off"}'
fi
