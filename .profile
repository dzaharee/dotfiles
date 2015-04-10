# .profile

export EDITOR=vim
export PAGER=less

export PATH=$HOME/bin:$PATH

if [ -f ~/.profile_local ]; then source ~/.profile_local; fi
