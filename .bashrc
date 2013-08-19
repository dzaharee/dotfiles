# .bashrc
#
# 2013-06-08

green=$(tput setaf 2)
yellow=$(tput setaf 3)
blue=$(tput setaf 4)
cyan=$(tput setaf 6)
reset=$(tput sgr0)

PS1='\[$cyan\]\u\[$yellow\]@\[$green\]\h\[$yellow\]:\[$blue\]\w\[$yellow\]\$ \[$reset\]'

export PS1

set -o vi

alias ls="ls -Ga --color=auto"
alias vi="vim"

eval `dircolors ~/.dir_colors`

source ~/.bashrc_local
