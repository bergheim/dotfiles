#!/bin/sh

if [ -d /etc/X11/xinit/xinitrc.d ]; then
	for f in /etc/X11/xinit/xinitrc.d/*; do
		[ -x "$f" ] && . "$f"
	done
	unset f
fi

~/.config/screenlayout/home.sh &

numlockx & # turn numlock on
unclutter & # hide the mouse when it is unused
clipmenud & # clipboard history
redshift & # dim the screen at night
dunst & # notifications

firefox-developer-edition &
spotify &
chromium &
telegram-desktop &

setxkbmap -option caps:escape # use caps as esc
setxkbmap -layout 'us,no' -option "grp:ctrls_toggle,terminate:ctrl_alt_bksp"
xset b off # don't beep
xset r rate 180 60
(sleep 5 && kbdd) & # kbdd is buggy if it does not start after the WM

feh --bg-fill ~/Pictures/wallpapers/active.png
~/.config/xautolock.sh &
picom -f &

session=${1:-bspwm}
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
