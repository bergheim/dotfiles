#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
	for f in /etc/X11/xinit/xinitrc.d/*; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

numlockx &
unclutter &
setxkbmap -option 'ctrl:nocaps' & # use caps as esc
xset r rate 180 40
# enable for extra logging
#exec i3 -V >> ~/i3log-$(date +'%F-%k-%M-%S') 2>&1
exec i3
