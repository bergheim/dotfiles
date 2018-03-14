#!/bin/sh

if [ "$(pidof spotify)" ]; then
  dbus_cmd="dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get string:org.mpris.MediaPlayer2.Player"
  # status can be Playing, Paused or Stopped
  status=`${dbus_cmd} string:'PlaybackStatus'|egrep -A 1 "string"|cut -b 26-|cut -d '"' -f 1|egrep -v ^$`

  if [ "$status" == "Playing" ]; then
    artist=`${dbus_cmd} string:'Metadata'|egrep -A 2 "artist"|egrep -v "artist"|egrep -v "array"|cut -b 27-|cut -d '"' -f 1|egrep -v ^$`
    album=`${dbus_cmd} string:'Metadata'|egrep -A 1 "album"|egrep -v "album"|cut -b 44-|cut -d '"' -f 1|egrep -v ^$`
    title=`${dbus_cmd} string:'Metadata'|egrep -A 1 "title"|egrep -v "title"|cut -b 44-|cut -d '"' -f 1|egrep -v ^$`

    echo ${artist} "-" ${title} | awk 'length > 60{$0=substr($0,0,61)"..."}1'
  else
    echo ""
  fi
else
  echo ""
fi
