# .bashrc
#
# 2012-01-25T18:53Z

export PS1="\[\e[36m\]\u\[\e[33m\]@\[\e[32m\]\h\[\e[33m\]:\[\e[34m\]\w\[\e[33m\]$ \[\e[0m\]" 

set -o vi

alias ls="ls -Ga"
alias vi="vim"

source ~/.bashrc_local
