#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
	for f in /etc/X11/xinit/xinitrc.d/*; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

numlockx &
unclutter &
clipmenud &
redshift-gtk &
dunst &
compton -f &
kbdd

setxkbmap -option caps:escape # use caps as esc
setxkbmap -layout 'us,no' -option "grp:ctrls_toggle,terminate:ctrl_alt_bksp"
xset b off # don't beep
xset r rate 180 50

if [ "$1" = "bspwm" ]; then
  sxhkd &
  # panel &
  polybar tb &
  exec bspwm
else
  exec i3
fi
