#!/bin/sh

start() {
  exec xautolock -detectsleep \
     -time 5 -locker "$HOME/local/bin/fancylocki3" \
     -notify 30 \
     -notifier "notify-send -u low -- 'LOCKING screen in 30 seconds'"
}

autolock_pid=`pidof xautolock`
if [ "$1" == "toggle" ]; then
  if [ -z "$autolock_pid" ]; then
    echo "start $autolock_pid"
    exec notify-send -u low -- "Starting xautolock" &
    notice clear
    exec xautolock -detectsleep \
         -time 5 -locker "$HOME/local/bin/fancylocki3" \
         -notify 30 \
         -notifier "notify-send -u low -- 'LOCKING screen in 30 seconds'" &
  else
    echo "stop $autolock_pid"
    notice "autolock disabled"
    exec notify-send -u low -- "Disabling xautolock" &
    exec kill "$autolock_pid"
  fi
elif [ -z "$autolock_pid" ]; then
  echo "Starting xautolock"
  notice clear
  start
fi
