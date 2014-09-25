#!/bin/bash
#
# .bashrc

# check for interactive
[[ $- = *i* ]] || return

# pretty prompt
green=$(tput setaf 2)
yellow=$(tput setaf 3)
blue=$(tput setaf 4)
cyan=$(tput setaf 6)
reset=$(tput sgr0)
export PS1='\[$cyan\]\u\[$yellow\]@\[$green\]\h\[$yellow\]:\[$blue\]\W\[$yellow\]\$ \[$reset\]'

# bash vi mode
set -o vi

# get aliases
source ~/.aliases

# use directory colours that work with Solarized
if hash dircolors 2>/dev/null; then
    eval `dircolors ~/.dir_colors`
fi

# load up local config if it exists
if [ -f ~/.bashrc_local ]; then source ~/.bashrc_local; fi

# Eternal bash history
# ---------------------
# Undocumented feature which sets the size to "unlimited".
# http://stackoverflow.com/questions/9457233/unlimited-bash-history
export HISTFILESIZE=
export HISTSIZE=
export HISTTIMEFORMAT="[%F %T] "
# Change the file location because certain bash sessions truncate .bash_history file upon close.
# http://superuser.com/questions/575479/bash-history-truncated-to-500-lines-on-each-login
export HISTFILE=~/.bash_eternal_history
# Force prompt to write history after every command.
# http://superuser.com/questions/20900/bash-history-loss
PROMPT_COMMAND="history -a; $PROMPT_COMMAND"
