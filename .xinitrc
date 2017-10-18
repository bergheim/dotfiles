#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
	for f in /etc/X11/xinit/xinitrc.d/*; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

numlockx &
unclutter &
setxkbmap -option caps:escape # use caps as esc
setxkbmap -layout 'us,no' -option grp:alt_shift_toggle
kbdd

xset b off # don't beep
xset r rate 180 40
# enable for extra logging
#exec i3 -V >> ~/i3log-$(date +'%F-%k-%M-%S') 2>&1
exec i3
