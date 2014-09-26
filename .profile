# .profile

export ALTERNATE_EDITOR=""
export EDITOR="TERM=xterm-16color emacsclient -tc"
export PAGER=less

export PATH=/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin:$HOME/bin

if [ -f ~/.profile_local ]; then source ~/.profile_local; fi
