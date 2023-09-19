#!/usr/bin/env bash

# this has issues with ssh terminal emacs
if [ "$EMACS" == "t" ]; then
    exec pinentry-emacs "$@"
elif [ -t 1 ]; then
    exec pinentry-curses "$@"
elif [ -n "$DISPLAY" ]; then
    exec pinentry-qt "$@"
else
    # Default to curses if none of the above conditions are met
    exec pinentry-curses "$@"
fi

# something like this miiight work..
# if [ "$EMACS" == "t" ] && [ -z "$DISPLAY" ]; then
#     # Inside terminal Emacs without a graphical display
#     # exec pinentry-curses "$@"
#     exec pinentry-tty "$@"
# elif [ "$EMACS" == "t" ] && [ -n "$DISPLAY" ]; then
#     # Inside graphical Emacs
#     exec pinentry-emacs "$@"
# elif [ -t 1 ]; then
#     # In a terminal
#     exec pinentry-curses "$@"
# elif [ -n "$DISPLAY" ]; then
#     # In an X11 environment outside Emacs
#     exec pinentry-gnome3 "$@"
# else
#     # Default to curses if none of the above conditions are met
#     exec pinentry-curses "$@"
# fi
