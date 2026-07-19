#!/bin/sh
# weather information for location based on seen WiFi BSSIDs or IP as fallback

# turn WiFi on if disabled
wifi_status="$(nmcli radio wifi)"
if [ "$wifi_status" = "disabled" ]; then
    nmcli device set wlp58s0 autoconnect no
    nmcli radio wifi on
fi

# get geographical location from close WiFi BSSIDs
for BSSID in $(nmcli device wifi list | awk 'NR>1 && NR<12 {if ($1 != "*") {print $1}}'); do
    # https://github.com/darkosancanin/apple_bssid_locator
    LOC="$(apple_bssid_locator "$BSSID" 2> /dev/null | tr -d '\n' |
        grep -Eo 'Latitude: [0-9.]+Longitude: [0-9.]+' |
        sed 's/Latitude: //' | sed 's/Longitude://')"
    if [ -n "$LOC" ]; then
        LOC="$(LC_ALL=C printf "%.2f,%.2f" $LOC)"
        break;
    fi
done

# turn WiFi off if it was disabled before
if [ "$wifi_status" = "disabled" ]; then
    nmcli radio wifi off
    nmcli device set wlp58s0 autoconnect yes
fi

# get weather information
text="$(curl -s "https://wttr.in/$LOC?format=1")"
tooltip="$(curl -s "https://wttr.in/$LOC?0QT" |
    sed 's/\\/\\\\/g' |
    sed ':a;N;$!ba;s/\n/\\n/g' |
    sed 's/"/\\"/g')"

# output for Waybar
if ! echo "$text" | grep -q "Unknown location"; then
    echo "{\"text\": \"$text\", \"tooltip\": \"<tt>$tooltip</tt>\", \"class\": \"weather\"}"
fi
