#!/bin/sh

# Make sure this is before the 'exec' command or it won't be sourced.
[ -f /etc/xprofile ] && . /etc/xprofile
[ -f ~/.xprofile ] && . ~/.xprofile

session=${1:-bspwm}
case $session in
    i3|i3wm)
        SXHKD_SHELL='/usr/bin/sh' sxhkd -c ~/.config/sxhkd/sxhkdrc.i3 ~/.config/sxhkd/sxhkdrc &
        exec i3;;
    bspwm)
        SXHKD_SHELL='/usr/bin/sh' sxhkd -c ~/.config/sxhkd/sxhkdrc.bspwm ~/.config/sxhkd/sxhkdrc &
        exec bspwm;;
    sway)
        SXHKD_SHELL='/usr/bin/sh' sxhkd -c ~/.config/sxhkd/sxhkdrc.i3 ~/.config/sxhkd/sxhkdrc &
        exec sway;;
    *)
        # No known session, try to run it as command
        exec $1;;
esac
