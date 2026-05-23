if [[ -z $DISPLAY && $XDG_VTNR -eq 1 ]]; then
    exec startx ~/.xinitrc i3
fi
