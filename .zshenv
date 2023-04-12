# Start configuration added by Zim install {{{
#
# User configuration sourced by all invocations of the shell
#

# Define Zim location
: ${ZIM_HOME=${ZDOTDIR:-${HOME}}/.zim}
# }}} End configuration added by Zim install

# export NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"

# Defer initialization of nvm until nvm, node or a node-dependent command is
# run. Ensure this block is only run once if .bashrc gets sourced multiple times
# by checking whether __init_nvm is a function.
# if [ -s "$HOME/.nvm/nvm.sh" ] && [ ! "$(type -t __init_nvm)" = function ]; then
#   export NVM_DIR="$HOME/.nvm"
#   # [ -s "$NVM_DIR/bash_completion" ] && . "$NVM_DIR/bash_completion"
#   declare -a __node_commands=('nvm' 'node' 'npm' 'yarn' 'gulp' 'grunt' 'webpack')
#   function __init_nvm() {
#     for i in "${__node_commands[@]}"; do unalias $i; done
#     . "$NVM_DIR"/nvm.sh
#     unset __node_commands
#     unset -f __init_nvm
#   }
#   for i in "${__node_commands[@]}"; do alias $i='__init_nvm && '$i; done
# fi

# typeset -U path
# path=(~/local/bin $path[@])
export PATH=~/local/bin:~/.local/bin:~/.cargo/bin:$PATH

export RANGER_LOAD_DEFAULT_RC=FALSE
export PANEL_FIFO="/tmp/panel-fifo"

export XDG_CONFIG_HOME="$HOME/.config"
export EDITOR=vim
export FCEDIT="$EDITOR"
export VISUAL="$EDITOR"
export SUDO_EDITOR="$EDITOR"

export MONITOR_PRIMARY="DP-2"
export MONITOR_SECONDARY="DP-0"

export QT_QPA_PLATFORMTHEME=qt5ct

# allow using the GnuPG agent as an SSH agent
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket) 
export GPG_TTY=$(tty)

export PAGER=bat

# Set temporary files locations
if [[ ! -d "$TMPDIR" ]]
then
    export TMPDIR="/tmp/$LOGNAME"
    mkdir -p -m 700 "$TMPDIR"
fi

export N_PREFIX=~/local
