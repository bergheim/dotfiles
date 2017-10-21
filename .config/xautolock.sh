#!/bin/sh

exec xautolock -detectsleep \
  -time 3 -locker "$HOME/local/bin/fancylocki3" \
  -notify 30 \
  -notifier "notify-send -t 29000 -- 'LOCKING screen in 30 seconds'"
