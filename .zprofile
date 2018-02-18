typeset -U path
path=(~/local/bin $path[@])

export RANGER_LOAD_DEFAULT_RC=FALSE
export PANEL_FIFO="/tmp/panel-fifo"

export XDG_CONFIG_HOME="$HOME/.config"
export EDITOR=vim
export FCEDIT="$EDITOR"
export VISUAL="$EDITOR"
export SUDO_EDITOR="$EDITOR"
