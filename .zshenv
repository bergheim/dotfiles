# Start configuration added by Zim install {{{
#
# User configuration sourced by all invocations of the shell
#

# Define Zim location
: ${ZIM_HOME=${ZDOTDIR:-${HOME}}/.zim}
# }}} End configuration added by Zim install

# Shared environment and PATH. This is also sourced by graphical and bash login
# sessions, so keep shell-neutral exports there.
[[ -r "$HOME/.profile" ]] && source "$HOME/.profile"

# Keep PATH/fpath deduplicated in nested zsh processes.
typeset -U path fpath
path=($path)

# This belongs to the current terminal, unlike the shared agent socket.
[[ -t 0 ]] && export GPG_TTY=$(tty)

# Avoid the distro-wide completion initialization; Zim owns it.
skip_global_compinit=1
