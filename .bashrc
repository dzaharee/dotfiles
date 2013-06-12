# .bashrc
#
# 2013-06-08

export PS1="\[\e[36m\]\u\[\e[33m\]@\[\e[32m\]\h\[\e[33m\]:\[\e[34m\]\w\[\e[33m\]$ \[\e[0m\]"

set -o vi

alias ls="ls -Ga --color=auto"
alias vi="vim"

eval `dircolors ~/.dir_colors`

source ~/.bashrc_local
