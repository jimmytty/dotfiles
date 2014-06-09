# /etc/profile: This file contains system-wide defaults used by
# all Bourne (and related) shells.

# Set the values for some environment variables:
export ASPELL_CONF="conf $HOME/.aspell.conf;personal $HOME/.aspell.pt_BR.pws; prefix $HOME/usr/"
export EDITOR='/usr/bin/vim'
export ELINKS_CACHE="$HOME/tmp/elinks_cache"
export FETCHMAILHOME="$HOME"
export FETCHMAILUSER="$USER"
export GREP_OPTIONS='--color=auto'
export HOSTNAME="`cat /etc/HOSTNAME`"
export HTML_TIDY="$HOME/.tidyrc"
export LESS="-M -R"
export LESSOPEN="|$HOME/usr/bin/lesspipe.sh %s"
export MAIL="$HOME/.mailbox/inbox"
export MAILPATH="$MAIL?Parece que chegou alguma coisa para voce em $_"
export MANPAGER='/usr/bin/less -is'
export MANPATH="/usr/local/man:/usr/man"
export MINICOM="-c on"
export PAGER='/usr/bin/less -r'
export PERL_CPANM_OPT="--mirror $HOME/minicpan/ --mirror-only --prompt --skip-installed --auto-cleanup 1 --quiet"
export PERL_HTML_DISPLAY_COMMAND='/usr/bin/elinks -remote "openURL(%s, new-tab)"'
export PERL_MM_USE_DEFAULT=1
export POD2TEXT_PARAMETERS='--alt --code --indent=4 --loose --quotes=\" --sentence --width=79'
export CPAN_MINI="$HOME/minicpan"
export BROWSER="$HOME/bin/browser"
export TMP_DIR="$HOME/tmp"
export TMPDIR="$HOME/tmp"
export VISUAL='/usr/bin/vim'
export ACRONYMDB="$HOME/usr/share/misc/acronyms"
export TZ='America/Sao_Paulo'
export SDCV_PAGER='piconv -f utf8 -t latin1 | fmt -w80 | less -R'

# If the user doesn't have a .inputrc, use the one in /etc.
if [ ! -r "$HOME/.inputrc" ]; then
    export INPUTRC=/etc/inputrc
fi

# Set the default system $PATH:
PATH="/usr/local/bin:/usr/bin:/bin:/usr/games"

# For root users, ensure that /usr/local/sbin, /usr/sbin, and /sbin are in
# the $PATH.  Some means of connection don't add these by default (sshd comes
# to mind).
if [ "`id -u`" = "0" ]; then
    echo $PATH | grep /usr/local/sbin 1> /dev/null 2> /dev/null
  if [ ! $? = 0 ]; then
        PATH=/usr/local/sbin:/usr/sbin:/sbin:$PATH
  fi
fi

# I had problems with the backspace key using 'eval tset' instead of 'TERM=',
# but you might want to try it anyway instead of the section below it.  I
# think with the right /etc/termcap it would work.
# eval `tset -sQ "$TERM"`

# Set TERM to linux for unknown type or unset variable:
if [ "$TERM" = "" -o "$TERM" = "unknown" ]; then
  TERM=linux
fi

## Set ksh93 visual editing mode:
#if [ "$SHELL" = "/bin/ksh" ]; then
#  VISUAL=emacs
##  VISUAL=gmacs
##  VISUAL=vi
#fi

# Set a default shell prompt:
#PS1='`hostname`:`pwd`# '
if [ "$SHELL" = "/bin/pdksh" ]; then
  PS1='! $ '
elif [ "$SHELL" = "/bin/ksh" ]; then
  PS1='! ${PWD/#$HOME/~}$ '
elif [ "$SHELL" = "/bin/zsh" ]; then
  PS1='%n@%m:%~%# '
elif [ "$SHELL" = "/bin/ash" ]; then
  PS1='$ '
else
  PS1='\u@\h:\w\$ '
fi
PS2='> '
PATH="$HOME/bin/:$HOME/perl5/bin/:$HOME/usr/bin/:/bin:/usr/bin:/usr/local/bin:/usr/games:/usr/share/texmf/bin:/usr/X11R6/bin/:/sbin:/usr/sbin:/usr/local/sbin:"
export PATH DISPLAY LESS TERM PS1 PS2

# Default umask.  A umask of 022 prevents new files from being created group
# and world writable.
# umask 022
umask 0077

# Notify user of incoming mail.  This can be overridden in the user's
# local startup file (~/.bash.login or whatever, depending on the shell)
if [ -x /usr/bin/biff ]; then
  biff y 2> /dev/null
fi

# Append any additional sh scripts found in /etc/profile.d/:
for profile_script in /etc/profile.d/*.sh ; do
  if [ -x $profile_script ]; then
    . $profile_script
  fi
done
unset profile_script

# For non-root users, add the current directory to the search path:
if [ ! "`id -u`" = "0" ]; then
  PATH="$PATH:."
fi

### SETs
/bin/setterm -bfreq 0
if [[ $TERM == "linux" ]]; then
  /usr/bin/setleds +num
fi
/usr/bin/mesg n
eval "$(dircolors $HOME/.dircolors)"

### PERL
source ~/perl5/perlbrew/etc/bashrc
perlbrew use perl-5.20.0
export MANPATH="$(/usr/bin/man -C ~/.man.conf -w):$HOME/usr/share/man/"

### ALIAS
alias ls="/bin/ls $LS_OPTIONS"
alias mc='. /usr/share/mc/bin/mc-wrapper.sh'
alias slocate='/usr/bin/slocate --database='"$HOME/var/lib/slocate/slocate.db"
alias shutdown='/usr/bin/sudo /sbin/shutdown'
alias xsh="VISUAL='vim -c '\''set syntax=html | set encoding=utf8'\''' xsh"
alias wicd='sudo /usr/bin/wicd-curses'

export PSQL_EDITOR='/usr/bin/vim -c '\''set nowrap syntax=sql'\'''
export XDG_CONFIG_HOME="$HOME"
export INFOPATH="$HOME/usr/share/info/:$HOME/usr/local/share/info/:"
export NETHACKDIR="$HOME/.nethack/"
export R_PROFILE="$HOME/R/rprofile.r"
