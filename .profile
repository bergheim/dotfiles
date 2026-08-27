# Shared environment for login sessions and shells. Keep this file POSIX sh.

path_prepend() {
    [ -d "$1" ] || return
    case ":$PATH:" in
        *":$1:"*) ;;
        *) PATH="$1${PATH:+:$PATH}" ;;
    esac
}

export GOBIN="$HOME/.local/bin"
export BUN_INSTALL="$HOME/.bun"
export PNPM_HOME="$HOME/.local/share/pnpm"

# Prepend in reverse priority order.
path_prepend "$HOME/bin"
path_prepend "$HOME/.cargo/bin"
path_prepend "$HOME/.local/bin"
path_prepend "$HOME/local/bin"
path_prepend "$BUN_INSTALL/bin"
path_prepend "$PNPM_HOME/bin"
path_prepend "$HOME/.local/share/mise/shims"
export PATH
unset -f path_prepend

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

export QT_AUTO_SCREEN_SCALE_FACTOR=1
export QT_QPA_PLATFORMTHEME=qt6ct

if [ -z "${SSH_AUTH_SOCK:-}" ] && command -v gpgconf >/dev/null 2>&1; then
    SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
    export SSH_AUTH_SOCK
fi

export PAGER=bat
export LESS='--ignore-case --jump-target=4 --LONG-PROMPT --quit-if-one-screen --RAW-CONTROL-CHARS'
export BROWSER=firefox

if [ -z "${TMPDIR:-}" ] || [ ! -d "$TMPDIR" ]; then
    export TMPDIR="/tmp/${LOGNAME:-$USER}"
    [ -d "$TMPDIR" ] || (umask 077 && mkdir -p "$TMPDIR")
fi

export N_PREFIX="$HOME/local"
export CANDLE_FLASH_ATTN_BUILD_DIR="$HOME/.cache/candle"
export XDG_MENU_PREFIX=arch-

if command -v podman >/dev/null 2>&1 && [ -n "${XDG_RUNTIME_DIR:-}" ]; then
    export DOCKER_HOST="unix://$XDG_RUNTIME_DIR/podman/podman.sock"
fi

export MOSH_ESCAPE_KEY=''

# shellcheck source=/dev/null
[ -r "$HOME/.zshenv-private" ] && . "$HOME/.zshenv-private"

[ -r "$HOME/.profile.container" ] && . "$HOME/.profile.container"
