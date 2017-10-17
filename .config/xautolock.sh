#!/bin/sh

exec xautolock -detectsleep \
  -time 3 -locker "xset s activate" \
  -notify 30 \
  -notifier "notify-send -t 29000 -- 'LOCKING screen in 30 seconds'"
