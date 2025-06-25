#!/bin/sh
json=$(
  gdbus call --system \
    --dest com.tuxedocomputers.tccd \
    --object-path /com/tuxedocomputers/tccd \
    --method com.tuxedocomputers.tccd.GetActiveProfileJSON |
    sed "s/^[(]'//; s/',)$//"
)

current_id=$(printf '%s' "$json" | jq -r .id)

# Check if on battery
on_battery=false
for battery in /sys/class/power_supply/BAT*; do
    if [ -f "$battery/status" ]; then
        status=$(cat "$battery/status")
        if [ "$status" = "Discharging" ]; then
            on_battery=true
            break
        fi
    fi
done


# Trim whitespace from profile name
# profile_name=$(
#   tux-power list | awk -v id="$current_id" '$1 == id {sub($1 FS, ""); gsub(/^[ \t]+|[ \t]+$/, ""); print}'
# )
# [ -z "$profile_name" ] && profile_name="$current_id"

# printf '{"text":"%s","tooltip":"Power Profile: %s"}\n' "$profile_name" "$profile_name"

case "$current_id" in
    "__default_custom_profile__"|"__legacy_default__")
        icon=""
        name="Performance"
        if [ "$on_battery" = true ]; then
            class="alert"
            tooltip="⚠️  Performance mode on battery - high drain!"
        else
            class=""
            tooltip="Power Profile: Performance (AC)"
        fi
        ;;
    "__legacy_cool_and_breezy__")
        icon=""
        name="Balanced"
        class=""
        tooltip="Power Profile: Balanced"
        ;;
    "__legacy_powersave_extreme__")
        icon=""
        name="Powersave"
        class=""
        tooltip="Power Profile: Powersave"
        ;;
    *)
        icon="?"
        name="Unknown"
        class=""
        tooltip="Power Profile: Unknown"
        ;;
esac

# printf '{"text":"%s","tooltip":"Power Profile: %s"}\n' "$icon" "$name"
printf '{"text":"%s","tooltip":"%s","class":"%s"}\n' "$icon" "$tooltip" "$class"

