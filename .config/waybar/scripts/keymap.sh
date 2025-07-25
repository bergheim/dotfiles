# #!/bin/bash
LAYOUT=$(swaymsg -t get_inputs | jq -r '.[] | select(.name=="AT Translated Set 2 keyboard") | .xkb_active_layout_name')
[[ "$LAYOUT" =~ "Norwegian" ]] && echo "🇳🇴" || echo "🇺🇸"
