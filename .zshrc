DEFAULT_USER=tsb # only show username, not username@hostname
if [[ ! -d ~/.zplug ]];then
    git clone https://github.com/b4b4r07/zplug ~/.zplug
fi

source ~/.zplug/init.zsh

zplug "zplug/zplug" # update yourself
zplug "lib/completion", from:oh-my-zsh # arrows to choose dir
zplug "plugins/command-not-found", from:oh-my-zsh
zplug "plugins/git",   from:oh-my-zsh
zplug "plugins/last-working-dir",   from:oh-my-zsh
zplug "supercrabtree/k" # use k to view git file status
zplug "b4b4r07/zsh-vimode-visual", use:"*.zsh", defer:3

# zplug "zsh-users/zsh-history-substring-search"
# zplug "joel-porquet/zsh-dircolors-solarized"

# z
# Navigate your most used directories based on 'frecency'.
# https://github.com/rupa/z
zplug 'rupa/z', use:'*.sh'

# Themes
autoload colors && colors
setopt prompt_subst # Make sure propt is able to be generated properly.

zplug "mafredri/zsh-async", from:github
zplug "dfurnes/purer", use:pure.zsh, from:github, as:theme

# zplug "themes/agnoster", from:oh-my-zsh
# zplug 'agnoster/agnoster-zsh-theme', as:theme
# zplug "bhilburn/powerlevel9k", use:powerlevel9k.zsh-theme
# zplug "adambiggs/zsh-theme", as:theme, use:adambiggs.zsh-theme

# Syntax highlighting for commands, load last
zplug "zsh-users/zsh-syntax-highlighting", from:github, defer:3

zplug check || zplug install
zplug load --verbose

# if zplug check joel-porquet/zsh-dircolors-solarized; then
#     setupsolarized dircolors.256dark
# fi

alias ls='\ls --color'
alias l='ls'
alias sl='ls'
alias la='ls -a'
alias ll='ls -l'
alias llh='ls -lh'

# enable zsh help. run with alt+h or esc+h FIXME does not work
autoload -Uz run-help
alias help=run-help

setopt no_beep

HISTFILE=~/.histfile
HISTSIZE=20000
SAVEHIST=20000
setopt extendedglob           # supports lots of globbing things
setopt hist_ignore_dups       # ignore duplication command history list
setopt hist_verify            # expand history onto the current line instead of executing it
setopt hist_expire_dups_first # when trimming history, lose oldest duplicates first
setopt hist_ignore_space      # don't save commands beginning with spaces to history
setopt extended_history       # save beginning time and elapsed time before commands in history
setopt append_history         # append to the end of the history file
setopt inc_append_history     # don't just save at termend
setopt share_history          # reloads the history whenever you use it
setopt hist_ignore_all_dups   # delete old recorded entry if new entry is a duplicate

# vim settings
bindkey -v
bindkey -M viins jj vi-cmd-mode
bindkey -M viins jk vi-cmd-mode
bindkey '^P' history-search-backward
bindkey '^N' history-search-forward
bindkey '^R' history-incremental-search-backward
bindkey '^S' history-incremental-search-forward
bindkey "^K" vi-up-line-or-history
bindkey "^J" vi-down-line-or-history
bindkey '^O' autosuggest-accept
bindkey '^A' beginning-of-line
bindkey '^E' end-of-line

bindkey '^[[H' beginning-of-line
bindkey '^[[F' end-of-line

# Edit command line with editor
autoload -Uz edit-command-line
zle -N edit-command-line
bindkey '^E' edit-command-line


# rehash executables after something is installed in $PATH
zstyle ':completion:*' rehash true

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
