. $HOME/.profile
bindkey -e
autoload -Uz compinit
setopt -o BRACE_CCL
setopt -o EXTENDED_GLOB
setopt -o RM_STAR_SILENT
setopt -o AUTO_PUSHD
setopt -o PUSHD_IGNORE_DUPS
compinit
HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000