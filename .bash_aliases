#! /bin/bash

alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF'
alias xcb='xclip -selection clipboard'
# use 'readline' for sqlite3 client
alias sqlite3='rlwrap sqlite3'
alias youtube-dl-mp3='youtube-dl -x --audio-format mp3 --prefer-ffmpeg'
alias boot-new="boot -d seancorfield/boot-new new"
alias boot-deps="boot -d boot-deps"
alias takeover="tmux detach -a"
alias ghc="stack ghc --"
# do not waste time switching between profiling and non-profiling build. See
# this Stack Overflow post:
# https://stackoverflow.com/questions/32123475/profiling-builds-with-stack
alias stack-profile="stack --work-dir .stack-work-profile --profile"
alias py-clean="find . -name \"*.py[c|o]\" -o -name __pycache__ | xargs rm -rf"
alias docker-compose-img="docker run --rm -it --name dcv -v $(pwd):/input pmsipilot/docker-compose-viz render -m image"
alias docker-name="docker ps --format '{{.Names}} : {{.Ports}}'"
alias yaml2json="python -c 'import sys, yaml, json; json.dump(yaml.load(sys.stdin), sys.stdout, indent=4)'"

alias aws-session="/opt/bin/aws-vault exec --session-ttl=8h --assume-role-ttl=1h --server developer --debug --backend=secret-service"

alias aws-exec="AWS_CONFIG_FILE=~/.aws_config aws-vault exec developer -- aws ${*:2}"




if [ "$1" == true ]; then
    alias ls='ls --color=auto --group-directories-first --classify'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'
    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

function strcat() {
  local IFS=""
  echo -n "$*"
}

# use like: sleep 10; alert
# TODO: split up long line
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'
