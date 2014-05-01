###############################################################################
# BASH CONFIGURATION FILE
###############################################################################

clear

source ~/.profile

PS1='\u@\h(\l):\w\$ '
export HISTCONTROL=erasedups:ignoreboth
export FCEDIT='/usr/bin/vim'
export FIGNORE=''
export GLOBIGNORE=''

shopt -s extglob
shopt -s cmdhist
shopt -s histappend
shopt -s histreedit
shopt -s histverify

