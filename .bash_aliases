#! /bin/bash

alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias xcb='xclip -selection clipboard'
# use 'readline' for sqlite3 client
alias sqlite3='rlwrap sqlite3'
alias youtube-dl-mp3='youtube-dl -x --audio-format mp3 --prefer-ffmpeg'

if [ "$1" == true ]; then
    alias ls='ls --color=auto --group-directories-first --classify'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# use like: sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
