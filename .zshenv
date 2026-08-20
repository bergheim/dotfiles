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

# keep PATH/fpath deduplicated -- .zshrc prepends to PATH, so without this every
# nested interactive shell (tmux inside emacs, etc) re-adds the same entries
typeset -U path fpath

export GOBIN="$HOME/.local/bin"
export BUN_INSTALL="$HOME/.bun"

# NB: array assignment, not `export PATH=...:$PATH`. typeset -U only uniquifies
# on *array* assignment; scalar PATH= assignments slip duplicates straight past
# it. Directories that don't exist on a given machine are harmless.
path=(
  "$BUN_INSTALL/bin"
  ~/local/bin
  ~/.local/bin          # == $GOBIN
  ~/.cargo/bin
  ~/bin
  $path
)

export RANGER_LOAD_DEFAULT_RC=FALSE
export PANEL_FIFO="/tmp/panel-fifo"

export LANG="en_US.UTF-8"
export LC_ALL="en_US.UTF-8"

export XDG_CONFIG_HOME="$HOME/.config"
export EDITOR=em
export FCEDIT="$EDITOR"
export VISUAL="$EDITOR"
export SUDO_EDITOR="$EDITOR"

export MONITOR_PRIMARY="DP-1"
export MONITOR_SECONDARY="DP-2"

# export GDK_SCALE=1.5
# export GDK_DPI_SCALE=0.5
export QT_AUTO_SCREEN_SCALE_FACTOR=1
# for qt5ct etc
# export QT_QPA_PLATFORMTHEME=qt5ct
export QT_QPA_PLATFORMTHEME=qt6ct

# allow using the GnuPG agent as an SSH agent
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
[ -t 0 ] && export GPG_TTY=$(tty)

export PAGER=bat
# zim's utility module sets this only if unset, so we win from .zshenv.
# Same as its default minus --no-init (-X), which breaks mouse-wheel
# scrolling and is only needed for less older than 530.
export LESS='--ignore-case --jump-target=4 --LONG-PROMPT --quit-if-one-screen --RAW-CONTROL-CHARS'
export BROWSER=firefox

# Set temporary files locations
if [[ ! -d "$TMPDIR" ]]
then
    export TMPDIR="/tmp/$LOGNAME"
    mkdir -p -m 700 "$TMPDIR"
fi

export N_PREFIX=~/local

# fix Ubuntu
skip_global_compinit=1

export CANDLE_FLASH_ATTN_BUILD_DIR=~/.cache/candle

# fix KDE associations
export XDG_MENU_PREFIX=arch-

source ~/.zshenv-private
