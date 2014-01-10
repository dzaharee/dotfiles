#!/bin/bash
#
# .bashrc
#
# 2013-10-15

# check for interactive
[[ $- = *i* ]] || return

# pretty prompt
green=$(tput setaf 2)
yellow=$(tput setaf 3)
blue=$(tput setaf 4)
cyan=$(tput setaf 6)
reset=$(tput sgr0)
export PS1='\[$cyan\]\u\[$yellow\]@\[$green\]\h\[$yellow\]:\[$blue\]\w\[$yellow\]\$ \[$reset\]'

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
