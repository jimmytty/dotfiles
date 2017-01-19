. $HOME/.profile
bindkey -e
autoload -Uz compinit
setopt -o AUTO_PUSHD
setopt -o BRACE_CCL
setopt -o EXTENDED_GLOB
setopt -o HIST_IGNORE_ALL_DUPS
setopt -o HIST_IGNORE_SPACE
setopt -o HIST_REDUCE_BLANKS
setopt -o HIST_SAVE_NO_DUPS
setopt -o PUSHD_IGNORE_DUPS
setopt -o RM_STAR_SILENT
compinit
HISTFILE=$HOME/.zsh_history
HISTSIZE=10000
SAVEHIST=10000