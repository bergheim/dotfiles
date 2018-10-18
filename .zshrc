DEFAULT_USER=tsb # only show username, not username@hostname

export ZIM_HOME=${ZDOTDIR:-${HOME}}/.zim
[[ -s ${ZIM_HOME}/init.zsh ]] && source ${ZIM_HOME}/init.zsh

# Themes
autoload colors && colors
setopt prompt_subst # Make sure propt is able to be generated properly.

case `uname` in
  Darwin)
	alias ls='ls -G'
	alias o='open'
  ;;
  Linux)
	alias ls='ls --color'
	alias o='xdg-open'
  ;;
esac

alias l='ls'
alias sl='ls'
alias la='ls -a'
alias ll='ls -l'
alias llh='ls -lh'

alias pacup='sudo pacman -Syu'
alias pacin='sudo pacman -S'
alias pacs='pacman -Ss'

# prefer vim to vi if it is installed
if type vim >/dev/null 2>/dev/null; then
  alias vi=vim
fi

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

# FIXME: c-l is clear...
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

# fzf things
export FZF_COMPLETION_TRIGGER="''"

if [ -f ~/.fzf.zsh ]; then
  source ~/.fzf.zsh
elif [ -d /usr/share/fzf ]; then
  source /usr/share/fzf/key-bindings.zsh
  source /usr/share/fzf/completion.zsh
fi

# use rg
# --files: List files that would be searched but do not search
# --no-ignore: Do not respect .gitignore, etc...
# --hidden: Search hidden files and folders
# --follow: Follow symlinks
# --glob: Additional conditions for search
export FZF_DEFAULT_COMMAND='rg --files --no-ignore --hidden --follow --glob "!{.git,node_modules}/*" 2> /dev/null'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
# preview
export FZF_CTRL_T_OPTS="--preview '(highlight -O ansi -l {} 2> /dev/null || cat {} || tree -C {}) 2> /dev/null | head -200'"
export FZF_ALT_C_OPTS="--preview 'tree -C {} | head -200'"

my_grep_options=(--color=auto --exclude-dir=.cvs --exclude-dir=.git --exclude-dir=.hg --exclude-dir=.svn)
alias grep='grep $my_grep_options' egrep='grep -E $my_grep_options' fgrep='grep -F $my_grep_options'

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
  local cols sep google_history open query
  cols=$(( COLUMNS / 3 ))
  sep='{::}'
  dbcopy="$HOME/tmp/c"
  filter=$@

  if [ "$(uname)" = "Darwin" ]; then
    google_history="$HOME/Library/Application Support/Google/Chrome/Default/History"
    open=open
  else
    google_history="$HOME/.config/chromium/Default/History"
    open=xdg-open
  fi
  cp -f "$google_history" "$dbcopy"

  query="select substr(title, 1, $cols), url from urls"
  if [ -n "${filter}" ]; then
    query="$query WHERE title LIKE '%$filter%' OR url LIKE '%$filter%'"
  fi
  query="$query order by last_visit_time desc"

  sqlite3 -separator $sep "$dbcopy" "$query" |
  awk -F $sep '{printf "%-'$cols's  \x1b[36m%s\x1b[m\n", $1, $2}' |
  fzf --ansi --multi | sed 's#.*\(https*://\)#\1#' | xargs $open > /dev/null 2> /dev/null
  rm -f "$dbcopy"
}

# f - browse firefox history
hf() {
  local cols sep firefox_history open filter
  cols=$(( COLUMNS / 3 ))
  sep='{::}'
  dbcopy="$HOME/tmp/f"
  filter=$@

  if [ "$(uname)" = "Darwin" ]; then
    firefox_history=(${HOME}/Library/Application\ Support/Firefox/**/*dev*/places.sqlite([1]N))

    if [ -z "$firefox_history" ]; then
      firefox_history=(${HOME}/Library/Application\ Support/Firefox/**/places.sqlite([1]N))
    fi
    open=open
  else
    firefox_history=(${HOME}/.mozilla/firefox/**/*dev*/places.sqlite([1]N))

    if [ -z "$firefox_history" ]; then
      firefox_history=(${HOME}/.mozilla/firefox/**/places.sqlite([1]N))
    fi
    open=xdg-open
  fi
  cp -f "$firefox_history" "$dbcopy"

  query="select p.title, p.url
    from moz_historyvisits as h, moz_places as p where p.id == h.place_id "
  if [ -n "${filter}" ]; then
    query="$query AND (p.title LIKE '%$filter%' OR p.url LIKE '%$filter%')"
  fi
  query="$query order by h.visit_date"

  sqlite3 -separator $sep "$dbcopy" "$query" |
    awk -F $sep '{printf "%-'$cols's  \x1b[36m%s\x1b[m\n", $1, $2}' |
    fzf --ansi --multi | sed 's#.*\(https*://\)#\1#' | xargs $open > /dev/null 2> /dev/null
  rm -f "$dbcopy"
}

nvm() {
  unset -f nvm
  export NVM_DIR=~/.nvm
  [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
  nvm "$@"
}

node() {
  unset -f node
  export NVM_DIR=~/.nvm
  [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
  node "$@"
}

npm() {
  unset -f npm
  export NVM_DIR=~/.nvm
  [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm
  npm "$@"
}

# attach and disconenct any current users (this enables resizing unlike tmux -A)
# if it does not exist, create it
# requires a string which is the session name
ta() {
  if [ -n "$1" ]
  then
    tmux attach -d -t $1 || tmux new -s $1
  else
    print "Please specify a session name"
  fi
}

# glf - git commit browser (enter for show, ctrl-d for diff, ` toggles sort)
glf() {
  local out shas sha q k
  while out=$(
      #--format="%C(auto)%h%d %s %C(black)%C(bold)%cr" "$@" |
      git log --graph --color=always \
          --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen[%ci (%cr)] %C(bold blue)<%an>%Creset' --abbrev-commit |
        fzf --ansi --multi --no-sort --reverse --query="${q}" --tiebreak=index \
            --print-query --expect=ctrl-d,ctrl-o --toggle-sort=\`); do
    q=$(head -1 <<< "${out}")
    k=$(head -2 <<< "${out}" | tail -1)
    shas=$(sed '1,2d;s/^[^a-z0-9]*//;/^$/d' <<< "${out}" | awk '{print $1}')
    [ -z "${shas}" ] && continue
    if [ "${k}" = 'ctrl-d' ] || [ "${k}" = 'ctrl-o' ]; then
      git diff --color=always ${shas} | less -R
    else
      for sha in ${shas}; do
        git show --color=always ${sha} | less -R
      done
    fi
  done
}
