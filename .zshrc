if [[ ! -d ~/.zplug ]];then
    git clone https://github.com/b4b4r07/zplug ~/.zplug
fi

source ~/.zplug/init.zsh

# Load completion library for those sweet [tab] squares
# zplug "lib/completion", from:oh-my-zsh
# Syntax highlighting for commands, load last
zplug "zsh-users/zsh-syntax-highlighting", from:github, defer:3

# Load completion library for those sweet [tab] squares
zplug "lib/completion", from:oh-my-zsh

zplug "plugins/command-not-found", from:oh-my-zsh

# use k to view git file status
zplug "supercrabtree/k"

zplug "b4b4r07/zsh-vimode-visual", use:"*.zsh", defer:3

zplug "joel-porquet/zsh-dircolors-solarized"

# zplug 'agnoster/agnoster-zsh-theme', as:theme
# pure prompt
#zplug "mafredri/zsh-async", from:github
#zplug "sindresorhus/pure", use:"pure.zsh", from:github, as:theme
zplug "bhilburn/powerlevel9k", use:powerlevel9k.zsh-theme

zplug load --verbose

# autoload -Uz colors
# colors

alias ls='\ls --color'
alias l='ls'
alias sl='ls'
alias la='ls -a'
alias ll='ls -l'
alias llh='ls -lh'

# if zplug check joel-porquet/zsh-dircolors-solarized; then
#     setupsolarized dircolors.256dark
# fi


# The following lines were added by compinstall
# zstyle :compinstall filename '/home/tsb/.zshrc'

# autoload -Uz compinit promptinit
# compinit
# promptinit

# enable zsh help. run with alt+h or esc+h FIXME does not work
autoload -Uz run-help
alias help=run-help

HISTFILE=~/.histfile
HISTSIZE=10000
SAVEHIST=10000
setopt appendhistory extendedglob

# vim settings
bindkey -v
bindkey -M viins jj vi-cmd-mode
bindkey -M viins jk vi-cmd-mode
bindkey '^P' history-search-backward
bindkey '^N' history-search-forward
bindkey '^R' history-incremental-search-backward
bindkey '^S' history-incremental-search-forward
bindkey "^k" vi-up-line-or-history
bindkey "^j" vi-down-line-or-history
bindkey '^o' autosuggest-accept

# enable arrows on completion
# zstyle ':completion:*' menu select
# setopt COMPLETE_ALIASES

# rehash executables after something is installed in $PATH
zstyle ':completion:*' rehash true

# search for package when "command not found"
# source /usr/share/doc/pkgfile/command-not-found.zsh

# enable additional syntax highlighting
# source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# navigate down/back with c-h, c-l
cdUndoKey() {
  popd
  zle       reset-prompt
  echo
  ls
  zle       reset-prompt
}

cdParentKey() {
  pushd ..
  zle      reset-prompt
  echo
  ls
  zle       reset-prompt
}

zle -N                 cdParentKey
zle -N                 cdUndoKey
bindkey '^[[1;3A'      cdParentKey
bindkey '^[[1;3D'      cdUndoKey
bindkey '^H'      cdParentKey
bindkey '^U'      cdUndoKey

## source ~/.config/zsh/prompt.sh


# fzf things
source /usr/share/fzf/key-bindings.zsh
source /usr/share/fzf/completion.zsh

# fd - cd to selected directory
fd() {
  local dir
  dir=$(find ${1:-.} -path '*/\.*' -prune \
                  -o -type d -print 2> /dev/null | fzf +m) &&
  cd "$dir"
}
# fda - including hidden directories
fda() {
  local dir
  dir=$(find ${1:-.} -type d 2> /dev/null | fzf +m) && cd "$dir"
}

# fkill - kill process
fkill() {
  local pid
  pid=$(ps -ef | sed 1d | fzf -m | awk '{print $2}')

  if [ "x$pid" != "x" ]
  then
    echo $pid | xargs kill -${1:-9}
  fi
}

# c - browse chrome history
hc() {
  local cols sep google_history open
  cols=$(( COLUMNS / 3 ))
  sep='{::}'

  if [ "$(uname)" = "Darwin" ]; then
    google_history="$HOME/Library/Application Support/Google/Chrome/Default/History"
    open=open
  else
    google_history="$HOME/.config/chromium/Default/History"
    open=xdg-open
  fi
  cp -f "$google_history" /tmp/h
  sqlite3 -separator $sep /tmp/h \
    "select substr(title, 1, $cols), url
     from urls order by last_visit_time desc" |
  awk -F $sep '{printf "%-'$cols's  \x1b[36m%s\x1b[m\n", $1, $2}' |
  fzf --ansi --multi | sed 's#.*\(https*://\)#\1#' | xargs $open > /dev/null 2> /dev/null
}
