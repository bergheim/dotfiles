#!/bin/sh
json=$(
  gdbus call --system \
    --dest com.tuxedocomputers.tccd \
    --object-path /com/tuxedocomputers/tccd \
    --method com.tuxedocomputers.tccd.GetActiveProfileJSON |
    sed "s/^[(]'//; s/',)$//"
)

current_id=$(printf '%s' "$json" | jq -r .id)

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
        ;;
    "__legacy_cool_and_breezy__")
        icon=""
        name="Balanced"
        ;;
    "__legacy_powersave_extreme__")
        icon=""
        name="Powersave"
        ;;
    *)
        icon="un"
        name="Unknown"
        ;;
esac

printf '{"text":"%s","tooltip":"Power Profile: %s"}\n' "$icon" "$name"

