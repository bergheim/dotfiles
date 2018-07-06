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
redshift &
dunst &
compton -f &

tilda &

setxkbmap -option caps:escape # use caps as esc
setxkbmap -layout 'us,no' -option "grp:ctrls_toggle,terminate:ctrl_alt_bksp"
xset b off # don't beep
xset r rate 180 50

feh --bg-fill ~/Pictures/wallpapers/active.png
~/.config/xautolock.sh &

session=${1:-i3}
case $session in
    i3|i3wm)
        SXHKD_SHELL='/usr/bin/sh' sxhkd -c ~/.config/sxhkd/sxhkdrc.i3 ~/.config/sxhkd/sxhkdrc &
        exec i3;;
    bspwm)
        SXHKD_SHELL='/usr/bin/sh' sxhkd -c ~/.config/sxhkd/sxhkdrc.bspwm ~/.config/sxhkd/sxhkdrc &
        exec bspwm;;
    *)
        # No known session, try to run it as command
        exec $1;;
esac
