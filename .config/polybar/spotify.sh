#!/bin/sh

# main() {
#   if ! pgrep -x spotify >/dev/null; then
#     echo ""; exit
#   fi
#
#   cmd="org.freedesktop.DBus.Properties.Get"
#   domain="org.mpris.MediaPlayer2"
#   path="/org/mpris/MediaPlayer2"
#
#   meta=$(dbus-send --print-reply --dest=${domain}.spotify \
#     /org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get string:${domain}.Player string:Metadata)
#
#   artist=$(echo "$meta" | sed -nr '/xesam:artist"/,+2s/^ +string "(.*)"$/\1/p' | tail -1  | sed "s/\&/+/g")
#   album=$(echo "$meta" | sed -nr '/xesam:album"/,+2s/^ +variant +string "(.*)"$/\1/p' | tail -1)
#   title=$(echo "$meta" | sed -nr '/xesam:title"/,+2s/^ +variant +string "(.*)"$/\1/p' | tail -1 | sed "s/\&/+/g")
#
#   echo "${*:-%artist% - %title%}" | sed "s/%artist%/$artist/g;s/%title%/$title/g;s/%album%/$album/g"i
# }
#
# main "$@"

#!/bin/bash
#
# Show Spotify current song

spotifyon=$(ps -e |grep spotify)
if [ -z "$spotifyon" ]; then
    echo ""
else
  artist=`dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get string:'org.mpris.MediaPlayer2.Player' string:'Metadata'|egrep -A 2 "artist"|egrep -v "artist"|egrep -v "array"|cut -b 27-|cut -d '"' -f 1|egrep -v ^$`
  title=`dbus-send --print-reply --dest=org.mpris.MediaPlayer2.spotify /org/mpris/MediaPlayer2 org.freedesktop.DBus.Properties.Get string:'org.mpris.MediaPlayer2.Player' string:'Metadata'|egrep -A 1 "title"|egrep -v "title"|cut -b 44-|cut -d '"' -f 1|egrep -v ^$`

  echo ${artist} "-" ${title} | awk 'length > 60{$0=substr($0,0,61)"..."}1'
fi
