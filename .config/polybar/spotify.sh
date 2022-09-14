#!/bin/sh

if [ "$(pidof spotify)" ]; then
  dbus_cmd="dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get string:org.mpris.MediaPlayer2.Player"
  # status can be Playing, Paused or Stopped
  status=`${dbus_cmd} string:'PlaybackStatus'|grep -A 1 -E "string"|cut -b 26-|cut -d '"' -f 1|grep -vE ^$`

  if [ "$status" != "Stopped" ]; then
    artist=`${dbus_cmd} string:'Metadata'|grep -A 2 -E "artist"|grep -vE "artist"|grep -vE "array"|cut -b 27-|cut -d '"' -f 1|grep -vE ^$`
    album=`${dbus_cmd} string:'Metadata'|grep -A 1 -E "album"|grep -vE "album"|cut -b 44-|cut -d '"' -f 1|grep -vE ^$`
    title=`${dbus_cmd} string:'Metadata'|grep -A 1 -E "title"|grep -vE "title"|cut -b 44-|cut -d '"' -f 1|grep -vE ^$`

    if [ "$status" == "Paused" ]; then
      echo -ne "[Paused] "
    fi
    echo ${artist} "-" ${title} | awk 'length > 60{$0=substr($0,0,61)"..."}1'
  else
    echo ""
  fi
else
  echo ""
fi
