source /usr/share/zsh/share/antigen.zsh
antigen use oh-my-zsh

antigen bundles <<EOBUNDLES
	git
	git-flow
#	history
#	vi-mode

	# Fish-like auto suggestions
	zsh-users/zsh-autosuggestions

	# Extra zsh completions
	zsh-users/zsh-completions

	command-not-found

	# Syntax highlighting bundle.
	zsh-users/zsh-syntax-highlighting
	zsh-users/zsh-history-substring-search
EOBUNDLES
antigen theme subnixr/minimal
#antigen theme robbyrussel

antigen apply

#bindkey -v
#bindkey -M viins jj vi-cmd-mode
#bindkey -M viins jk vi-cmd-mode
#bindkey "^k" vi-up-line-or-history
#bindkey "^j" vi-down-line-or-history


# The following lines were added by compinstall
zstyle :compinstall filename '/home/tsb/.zshrc'

autoload -Uz compinit promptinit
compinit
promptinit

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
zstyle ':completion:*' menu select
setopt COMPLETE_ALIASES

# rehash executables after something is installed in $PATH
zstyle ':completion:*' rehash true

# history search FIXME does not work
# autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
# zle -N up-line-or-beginning-search
# zle -N down-line-or-beginning-search
# [[ -n "$key[Up]"   ]] && bindkey -- "$key[Up]"   up-line-or-beginning-search
# [[ -n "$key[Down]" ]] && bindkey -- "$key[Down]" down-line-or-beginning-search

# search for package when "command not found"
source /usr/share/doc/pkgfile/command-not-found.zsh

# enable additional syntax highlighting
source /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# remember previous dirs - list them with dirs -v
# DIRSTACKFILE="$HOME/.cache/zsh/dirs"
# if [[ -f $DIRSTACKFILE ]] && [[ $#dirstack -eq 0 ]]; then
#   dirstack=( ${(f)"$(< $DIRSTACKFILE)"} )
#   [[ -d $dirstack[1] ]] && cd $dirstack[1]
# fi
# chpwd() {
#   print -l $PWD ${(u)dirstack} >$DIRSTACKFILE
# }
# 
# DIRSTACKSIZE=20
# 
# setopt AUTO_PUSHD PUSHD_SILENT PUSHD_TO_HOME
# 
# ## Remove duplicate entries
# setopt PUSHD_IGNORE_DUPS
# 
# ## This reverts the +/- operators.
# setopt PUSHD_MINUS

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
