# .profile

export EDITOR="TERM=xterm-16color emacs -nw"
export PAGER=less

export PATH=/sbin:/bin:/usr/sbin:/usr/bin:/usr/local/sbin:/usr/local/bin:$HOME/bin

if [ -f ~/.profile_local ]; then source ~/.profile_local; fi
