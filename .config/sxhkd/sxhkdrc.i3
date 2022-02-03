#!/usr/bin/env sh
# exit i3 (logs you out of your X session)
ctrl + alt + Delete
  exec sh -c '[ $(echo -e "NO\nYES" | dmenu -sb "#b89000" -i -p "Really exit i3 X session?") = "YES" ] && i3-msg exit'
